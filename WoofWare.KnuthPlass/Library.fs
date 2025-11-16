namespace WoofWare.KnuthPlass

/// Represents a fixed-width item (word, character, etc.)
type Box =
    {
        Width : float
    }

/// Represents stretchable/shrinkable whitespace
type Glue =
    {
        Width : float
        Stretch : float
        Shrink : float
    }

/// Represents a potential break point with associated cost
type Penalty =
    {
        Width : float
        Cost : float
        /// True for flagged penalties (e.g., hyphenated breaks)
        Flagged : bool
    }

/// An item in the paragraph to be broken into lines
type Item =
    | Box of Box
    | Glue of Glue
    | Penalty of Penalty

/// Represents a line in the output
type Line =
    {
        /// Index of first item in this line
        Start : int
        /// Index of last item before the break (exclusive)
        End : int
        /// Adjustment ratio: how much glue was stretched (positive) or compressed (negative)
        AdjustmentRatio : float
    }

/// Options for line breaking
type LineBreakOptions =
    {
        /// Target width for each line
        LineWidth : float
        /// Tolerance for acceptable lines (typically 1-3). Higher values allow looser lines.
        Tolerance : float
        /// Penalty for consecutive lines of very different tightness
        AdjacentLooseTightDemerits : float
        /// Penalty for consecutive hyphenated lines
        DoubleHyphenDemerits : float
        /// Penalty for ending a paragraph with a hyphen
        FinalHyphenDemerits : float
        /// Penalty multiplier for fitness class differences
        FitnessClassDifferencePenalty : float
    }

    /// Creates default options with standard TeX-like values
    static member Default (lineWidth : float) =
        {
            LineWidth = lineWidth
            Tolerance = 10.0 // Allow significant stretch/shrink
            AdjacentLooseTightDemerits = 10000.0
            DoubleHyphenDemerits = 10000.0
            FinalHyphenDemerits = 5000.0
            FitnessClassDifferencePenalty = 100.0
        }

module LineBreaker =
    type FitnessClass =
        | Tight
        | Normal
        | Loose
        | VeryLoose

    type BreakNode =
        {
            Position : int
            Demerits : float
            Ratio : float
            PreviousNode : int option
            Fitness : FitnessClass
            WasFlagged : bool
        }

    /// Precomputed cumulative sums for efficient line computation
    type CumulativeSums =
        {
            Width : float[]
            Stretch : float[]
            Shrink : float[]
        }

    let private computeCumulativeSums (items : Item list) : CumulativeSums =
        let arr = items |> List.toArray
        let n = arr.Length
        let width = Array.zeroCreate (n + 1)
        let stretch = Array.zeroCreate (n + 1)
        let shrink = Array.zeroCreate (n + 1)

        for i in 0 .. n - 1 do
            width.[i + 1] <- width.[i]
            stretch.[i + 1] <- stretch.[i]
            shrink.[i + 1] <- shrink.[i]

            match arr.[i] with
            | Box b -> width.[i + 1] <- width.[i + 1] + b.Width
            | Glue g ->
                width.[i + 1] <- width.[i + 1] + g.Width
                stretch.[i + 1] <- stretch.[i + 1] + g.Stretch
                shrink.[i + 1] <- shrink.[i + 1] + g.Shrink
            | Penalty p -> width.[i + 1] <- width.[i + 1] + p.Width

        {
            Width = width
            Stretch = stretch
            Shrink = shrink
        }

    /// Compute adjustment ratio for a line from startIdx to endIdx
    let private computeAdjustmentRatio
        (sums : CumulativeSums)
        (lineWidth : float)
        (startIdx : int)
        (endIdx : int)
        : float option
        =
        let actualWidth = sums.Width.[endIdx] - sums.Width.[startIdx]
        let diff = lineWidth - actualWidth

        if abs diff < 1e-10 then
            Some 0.0
        elif diff > 0.0 then
            // Line is too short, need to stretch
            let totalStretch = sums.Stretch.[endIdx] - sums.Stretch.[startIdx]

            if totalStretch > 0.0 then
                Some (diff / totalStretch)
            else
                // No glue to stretch, but line is underfull - this is acceptable
                // Return a very small positive ratio to indicate underfull
                Some 0.0
        else
            // Line is too long, need to compress
            let totalShrink = sums.Shrink.[endIdx] - sums.Shrink.[startIdx]

            if totalShrink > 0.0 then
                Some (diff / totalShrink)
            else
                // No glue to shrink and line is overfull - cannot fit
                None

    let private fitnessClass (ratio : float) : FitnessClass =
        if ratio < -0.5 then Tight
        elif ratio <= 0.5 then Normal
        elif ratio <= 1.0 then Loose
        else VeryLoose

    let private badness (ratio : float) : float =
        let r = abs ratio
        100.0 * (r ** 3.0)

    let private computeDemerits
        (options : LineBreakOptions)
        (ratio : float)
        (penaltyCost : float)
        (prevFitness : FitnessClass)
        (currFitness : FitnessClass)
        (prevWasFlagged : bool)
        (currIsFlagged : bool)
        (isLastLine : bool)
        : float
        =
        let bad = badness ratio
        let linePenalty = (1.0 + bad) + abs penaltyCost
        let mutable demerits = linePenalty * linePenalty

        // Penalty for consecutive flagged breaks (double hyphen)
        if prevWasFlagged && currIsFlagged then
            demerits <- demerits + options.DoubleHyphenDemerits

        // Penalty for fitness class mismatch
        if prevFitness <> currFitness then
            demerits <- demerits + options.FitnessClassDifferencePenalty

        // Penalty for ending with a flagged break
        if isLastLine && currIsFlagged then
            demerits <- demerits + options.FinalHyphenDemerits

        demerits

    /// In Knuth-Plass, we can break at the following positions:
    /// 1. After any glue
    /// 2. At any penalty
    /// 3. At the end of the paragraph
    let private isValidBreakpoint (itemsArray : Item array) (idx : int) : bool =
        if idx = 0 then
            true // Start of paragraph
        elif idx >= itemsArray.Length then
            true // End of paragraph - always a valid breakpoint
        elif idx > 0 && idx <= itemsArray.Length then
            // Look at the item just before this position
            match itemsArray.[idx - 1] with
            | Glue _ -> true // Can break after glue
            | Penalty _ -> true // Can break at penalty
            | Box _ -> false // Cannot break after a box
        else
            false

    let private getPenaltyAt (itemsArray : Item array) (idx : int) : float * bool =
        if idx >= itemsArray.Length then
            (0.0, false) // No penalty at end of paragraph
        elif idx > 0 then
            // Check if previous item was a penalty
            match itemsArray.[idx - 1] with
            | Penalty p -> (p.Cost, p.Flagged)
            | _ -> (0.0, false)
        else
            (0.0, false)

    /// Break a paragraph into lines using the Knuth-Plass algorithm.
    /// Returns a list of lines with their start/end positions and adjustment ratios.
    /// Raises an exception if no valid breaking is possible.
    let breakLines (options : LineBreakOptions) (items : Item list) : Line list =
        if items.IsEmpty then
            []
        else
            let itemsArray = items |> List.toArray
            let n = itemsArray.Length
            let sums = computeCumulativeSums items

            // Track best node at each position for each fitness class
            let bestNodes = System.Collections.Generic.Dictionary<int * FitnessClass, int> ()

            // All nodes for backtracking
            let nodes = ResizeArray<BreakNode> ()

            // Start node at position 0
            nodes.Add
                {
                    Position = 0
                    Demerits = 0.0
                    Ratio = 0.0
                    PreviousNode = None
                    Fitness = Normal
                    WasFlagged = false
                }

            bestNodes.[(0, Normal)] <- 0

            // Track the last forced break position to prevent skipping over it
            let mutable lastForcedBreak = -1

            // For each position, find the best predecessor
            for i in 1..n do
                if isValidBreakpoint itemsArray i then
                    let penaltyCost, isFlagged = getPenaltyAt itemsArray i
                    let isForced = penaltyCost = -infinity

                    // Try all previous nodes as potential predecessors
                    for prevNodeIdx in 0 .. nodes.Count - 1 do
                        let prevNode = nodes.[prevNodeIdx]
                        let startPos = prevNode.Position

                        // Only consider nodes at earlier positions
                        // and don't skip over forced breaks
                        if startPos < i && startPos >= lastForcedBreak then
                            match computeAdjustmentRatio sums options.LineWidth startPos i with
                            | Some ratio when isForced || ratio >= 0.0 || abs ratio <= options.Tolerance ->
                                // Accept if: forced break, underfull (ratio >= 0), or within tolerance
                                let fitness = fitnessClass ratio
                                let isLast = i = n

                                let demerits =
                                    prevNode.Demerits
                                    + computeDemerits
                                        options
                                        ratio
                                        penaltyCost
                                        prevNode.Fitness
                                        fitness
                                        prevNode.WasFlagged
                                        isFlagged
                                        isLast

                                // Check if this is better than existing node at this position/fitness
                                let key = (i, fitness)

                                let shouldAdd =
                                    match bestNodes.TryGetValue key with
                                    | true, existingIdx -> demerits < nodes.[existingIdx].Demerits
                                    | false, _ -> true

                                if shouldAdd then
                                    let newNode =
                                        {
                                            Position = i
                                            Demerits = demerits
                                            Ratio = ratio
                                            PreviousNode = Some prevNodeIdx
                                            Fitness = fitness
                                            WasFlagged = isFlagged
                                        }

                                    nodes.Add newNode
                                    bestNodes.[key] <- nodes.Count - 1

                            | _ -> () // Line doesn't fit

                    // If this is a forced break, update the last forced break position
                    if isForced then
                        lastForcedBreak <- i

            // Find best ending node
            let endNodes =
                nodes
                |> Seq.indexed
                |> Seq.filter (fun (_, node) -> node.Position = n)
                |> Seq.toList

            match endNodes with
            | [] -> failwith "No valid line breaking found"
            | _ ->
                let bestEndIdx, _ = endNodes |> List.minBy (fun (_, node) -> node.Demerits)

                // Backtrack to recover the solution
                let rec backtrack acc nodeIdx =
                    let node = nodes.[nodeIdx]

                    match node.PreviousNode with
                    | None -> acc
                    | Some prevIdx ->
                        let prevNode = nodes.[prevIdx]

                        let line =
                            {
                                Start = prevNode.Position
                                End = node.Position
                                AdjustmentRatio = node.Ratio
                            }

                        backtrack (line :: acc) prevIdx

                backtrack [] bestEndIdx

module Items =
    /// Creates a box with the given width
    let box (width : float) : Item =
        Box
            {
                Width = width
            }

    /// Creates glue with the given width, stretch, and shrink values
    let glue (width : float) (stretch : float) (shrink : float) : Item =
        Glue
            {
                Width = width
                Stretch = stretch
                Shrink = shrink
            }

    /// Creates a penalty with the given width, cost, and flagged status
    let penalty (width : float) (cost : float) (flagged : bool) : Item =
        Penalty
            {
                Width = width
                Cost = cost
                Flagged = flagged
            }

    /// Creates a forced break (infinite penalty against not breaking)
    let forcedBreak () : Item = penalty 0.0 (-infinity) false

    /// Converts a simple string into a list of items (boxes for words, glue for spaces)
    let fromString (text : string) (wordWidth : string -> float) (spaceWidth : float) : Item list =
        let words = text.Split ([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)

        [
            for i, word in Array.indexed words do
                yield box (wordWidth word)

                // Add glue between words (but not after the last word)
                if i < words.Length - 1 then
                    yield glue spaceWidth (spaceWidth * 0.5) (spaceWidth * 0.333)
        ]
