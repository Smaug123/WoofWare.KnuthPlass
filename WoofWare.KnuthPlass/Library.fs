namespace WoofWare.KnuthPlass

open System

[<RequireQualifiedAccess>]
module LineBreaker =
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
        : float voption
        =
        let actualWidth = sums.Width.[endIdx] - sums.Width.[startIdx]
        let diff = lineWidth - actualWidth

        if abs diff < 1e-10 then
            ValueSome 0.0
        elif diff > 0.0 then
            // Line is too short, need to stretch
            let totalStretch = sums.Stretch.[endIdx] - sums.Stretch.[startIdx]

            if totalStretch > 0.0 then
                ValueSome (diff / totalStretch)
            else
                // No glue to stretch, but line is underfull - this is acceptable
                // Return a very small positive ratio to indicate underfull
                ValueSome 0.0
        else
            // Line is too long, need to compress
            let totalShrink = sums.Shrink.[endIdx] - sums.Shrink.[startIdx]

            if totalShrink > 0.0 then
                ValueSome (diff / totalShrink)
            else
                // No glue to shrink and line is overfull - cannot fit
                ValueNone

    let inline private fitnessClass (ratio : float) : FitnessClass =
        if ratio < -0.5 then FitnessClass.Tight
        elif ratio <= 0.5 then FitnessClass.Normal
        elif ratio <= 1.0 then FitnessClass.Loose
        else FitnessClass.VeryLoose

    let inline private badness (ratio : float) : float =
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

            // Track the best node at each position for each fitness class.
            // A value of IntMin means "not set".
            let bestNodes = Array.create<int> (4 * (n + 1)) Int32.MinValue

            let inline getBestNode (f : FitnessClass) (pos : int) =
                bestNodes.[(n + 1) * int<FitnessClass> f + pos]

            let inline setBestNode (f : FitnessClass) (pos : int) (v : int) =
                bestNodes.[(n + 1) * int<FitnessClass> f + pos] <- v

            // All nodes for backtracking
            let nodes = ResizeArray<BreakNode> ()

            // Start node at position 0
            nodes.Add
                {
                    Position = 0
                    Demerits = 0.0
                    Ratio = 0.0
                    PreviousNode = None
                    Fitness = FitnessClass.Normal
                    WasFlagged = false
                }

            setBestNode FitnessClass.Normal 0 0

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
                            | ValueSome ratio when isForced || ratio >= 0.0 || abs ratio <= options.Tolerance ->
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
                                let shouldAdd =
                                    let result = getBestNode fitness i

                                    if result = Int32.MinValue then
                                        true
                                    else
                                        demerits < nodes.[result].Demerits

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
                                    setBestNode fitness i (nodes.Count - 1)

                            | _ -> () // Line doesn't fit

                    // If this is a forced break, update the last forced break position
                    if isForced then
                        lastForcedBreak <- i

            // Find best ending node
            let bestEndIdx =
                nodes
                |> Seq.indexed
                |> Seq.filter (fun (_, node) -> node.Position = n)
                |> Seq.tryMinBy (fun (_, node) -> node.Demerits)
                |> Option.map fst

            match bestEndIdx with
            | None -> failwith "No valid line breaking found"
            | Some bestEndIdx ->
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
        let words =
            text.Replace("\r", "").Split ([| ' ' ; '\n' |], StringSplitOptions.RemoveEmptyEntries)

        [
            for i, word in Array.indexed words do
                yield box (wordWidth word)

                // Add glue between words (but not after the last word)
                if i < words.Length - 1 then
                    yield glue spaceWidth (spaceWidth * 0.5) (spaceWidth * 0.333)
        ]

[<RequireQualifiedAccess>]
module Format =
    let private defaultWordWidth (s : string) = float s.Length

    /// Formats text into a paragraph with line breaks using the Knuth-Plass algorithm.
    /// Returns the text with line breaks inserted at 'optimal' positions.
    let formatParagraph'
        (lineWidth : float)
        (wordWidth : string -> float)
        (spaceWidth : float)
        (text : string)
        : string
        =
        let words = text.Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

        if words.Length = 0 then
            ""
        else
            let items = Items.fromString text wordWidth spaceWidth
            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items

            // Extract words for each line
            // Items alternate: box (even indices) and glue (odd indices)
            // Word i is at item index i*2
            let lineTexts =
                lines
                |> List.map (fun line ->
                    // Get all box indices in this line
                    [ line.Start .. line.End - 1 ]
                    |> List.filter (fun i -> i % 2 = 0) // Boxes are at even indices
                    |> List.map (fun i -> words.[i / 2])
                    |> String.concat " "
                )

            String.concat Environment.NewLine lineTexts

    /// Formats text into a paragraph with line breaks using the Knuth-Plass algorithm.
    /// Returns the text with line breaks inserted at 'optimal' positions.
    let formatParagraph (lineWidth : float) (text : string) : string =
        formatParagraph' lineWidth defaultWordWidth 1.0 text
