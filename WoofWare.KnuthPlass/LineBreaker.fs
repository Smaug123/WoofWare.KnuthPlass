namespace WoofWare.KnuthPlass

open System

type private BreakNode =
    {
        Position : int
        Demerits : float
        Ratio : float
        PreviousNode : int option
        Fitness : FitnessClass
        WasFlagged : bool
    }

/// Precomputed cumulative sums for efficient line computation
type private CumulativeSums =
    {
        Width : float[]
        Stretch : float[]
        Shrink : float[]
    }

/// The module holding the heart of the Knuth-Plass algorithm.
[<RequireQualifiedAccess>]
module LineBreaker =
    /// A very large adjustment ratio used when a line is underfull (too short) but has no glue
    /// to stretch. This value ensures the line gets categorized as VeryLoose with high badness,
    /// making it an unattractive but still feasible breaking option. This is used when we must
    /// represent extreme looseness in the algorithm's dynamic programming calculations.
    [<Literal>]
    let private extremeLooseRatio = 1000.0

    /// Maximum excess tolerance value to prevent overflow when computing the quadratic penalty
    /// (excess * excess) for tolerance violations. This cap ensures that extremely bad lines
    /// still receive severe penalties without causing numerical overflow or producing demerits
    /// values so large they dominate all other algorithmic considerations. With this cap, the
    /// maximum penalty contribution from tolerance violations is maxExcessTolerance^2.
    [<Literal>]
    let private maxExcessTolerance = 10000.0

    let private computeCumulativeSums (items : Item[]) : CumulativeSums =
        let n = items.Length
        let width = Array.zeroCreate (n + 1)
        let stretch = Array.zeroCreate (n + 1)
        let shrink = Array.zeroCreate (n + 1)

        for i in 0 .. n - 1 do
            width.[i + 1] <- width.[i]
            stretch.[i + 1] <- stretch.[i]
            shrink.[i + 1] <- shrink.[i]

            match items.[i] with
            | Box b -> width.[i + 1] <- width.[i + 1] + b.Width
            | Glue g ->
                width.[i + 1] <- width.[i + 1] + g.Width
                stretch.[i + 1] <- stretch.[i + 1] + g.Stretch
                shrink.[i + 1] <- shrink.[i + 1] + g.Shrink
            | Penalty _ -> ()

        {
            Width = width
            Stretch = stretch
            Shrink = shrink
        }

    /// Compute adjustment ratio for a line from startIdx to endIdx
    let private computeAdjustmentRatio
        (itemsArray : Item array)
        (sums : CumulativeSums)
        (lineWidth : float)
        (startIdx : int)
        (endIdx : int)
        : float voption
        =
        let mutable actualWidth = sums.Width.[endIdx] - sums.Width.[startIdx]
        let mutable totalStretch = sums.Stretch.[endIdx] - sums.Stretch.[startIdx]
        let mutable totalShrink = sums.Shrink.[endIdx] - sums.Shrink.[startIdx]

        if endIdx > 0 && endIdx <= itemsArray.Length then
            match itemsArray.[endIdx - 1] with
            | Penalty p -> actualWidth <- actualWidth + p.Width
            | _ -> ()

        let diff = lineWidth - actualWidth

        if abs diff < 1e-10 then
            ValueSome 0.0
        elif diff > 0.0 then
            // Line is too short, need to stretch
            if totalStretch > 0.0 then
                ValueSome (diff / totalStretch)
            else
                // No glue to stretch, but line is underfull
                // Return a very large ratio to indicate extreme looseness
                // This ensures the line gets categorized as VeryLoose with high badness
                ValueSome extremeLooseRatio
        else if
            // Line is too long, need to compress
            totalShrink > 0.0
        then
            ValueSome (diff / totalShrink)
        else
            // No glue to shrink and line is overfull - cannot fit
            ValueNone

    /// Once a layout has been chosen we display the per-line ratio as TeX would perceive it,
    /// which means discarding any trailing glue at the breakpoint.
    let private computeDisplayedAdjustmentRatio
        (itemsArray : Item array)
        (sums : CumulativeSums)
        (lineWidth : float)
        (startIdx : int)
        (endIdx : int)
        : float
        =
        let mutable actualWidth = sums.Width.[endIdx] - sums.Width.[startIdx]
        let mutable totalStretch = sums.Stretch.[endIdx] - sums.Stretch.[startIdx]
        let mutable totalShrink = sums.Shrink.[endIdx] - sums.Shrink.[startIdx]

        if endIdx > 0 && endIdx <= itemsArray.Length then
            match itemsArray.[endIdx - 1] with
            | Glue g ->
                actualWidth <- actualWidth - g.Width
                totalStretch <- totalStretch - g.Stretch
                totalShrink <- totalShrink - g.Shrink
            | Penalty p -> actualWidth <- actualWidth + p.Width
            | _ -> ()

        let diff = lineWidth - actualWidth

        if abs diff < 1e-10 then
            0.0
        elif diff > 0.0 then
            if totalStretch > 0.0 then
                diff / totalStretch
            else
                extremeLooseRatio
        else if totalShrink > 0.0 then
            diff / totalShrink
        else
            failwithf
                "Internal error: line %d..%d is overfull (needs %.2f shrink but has 0.0 available) and should have been rejected during feasibility check"
                startIdx
                endIdx
                (-diff)

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
        let linePenalty = 1.0 + bad

        // Compute base demerits according to Knuth-Plass formula:
        // - Forced breaks (penalty -infinity): use L² only
        // - Positive penalties P ≥ 0: use (L + P)²
        // - Negative penalties P < 0: use L² - P² (encourages breaking)
        let mutable demerits =
            if penaltyCost = -infinity then
                linePenalty * linePenalty
            elif penaltyCost >= 0.0 then
                let totalPenalty = linePenalty + penaltyCost
                totalPenalty * totalPenalty
            else
                // Negative penalty reduces demerits
                (linePenalty * linePenalty) - (penaltyCost * penaltyCost)

        // Tolerance violation penalty: when badness exceeds the allowed tolerance,
        // apply a quadratic penalty (badness - tolerance)^2 to strongly discourage
        // extreme lines. This penalty grows rapidly with increasing violations,
        // making grossly over/under-full lines significantly less attractive than
        // slightly over-tolerance lines, even if other penalties would otherwise
        // favor them.
        if bad > options.Tolerance then
            let excess = min (bad - options.Tolerance) maxExcessTolerance
            demerits <- demerits + (excess * excess)

        // Penalty for consecutive flagged breaks (double hyphen)
        if prevWasFlagged && currIsFlagged then
            demerits <- demerits + options.DoubleHyphenDemerits

        // Penalty for fitness class mismatch
        if prevFitness <> currFitness then
            let fitnessDiff = abs (int prevFitness - int currFitness)
            // Large penalty for non-adjacent fitness classes (e.g., Tight to Loose)
            if fitnessDiff > 1 then
                demerits <- demerits + options.AdjacentLooseTightDemerits
            else
                demerits <- demerits + options.FitnessClassDifferencePenalty

        // Penalty for ending with a flagged break
        if isLastLine && prevWasFlagged then
            demerits <- demerits + options.FinalHyphenDemerits

        demerits

    /// In Knuth-Plass, we can break at the following positions:
    /// 1. After any glue (but not between two consecutive glues)
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
            | Glue _ ->
                // Can break after glue, but not if the next item is also glue
                if idx < itemsArray.Length then
                    match itemsArray.[idx] with
                    | Glue _ -> false // Cannot break between two consecutive glues
                    | _ -> true
                else
                    true // At end of array
            | Penalty _ -> true // Can break at penalty
            | Box _ -> false // Cannot break after a box
        else
            false

    let private getPenaltyAt (itemsArray : Item array) (idx : int) : float * bool =
        if idx > 0 && idx <= itemsArray.Length then
            match itemsArray.[idx - 1] with
            | Penalty p -> p.Cost, p.Flagged
            | _ -> 0.0, false
        else
            0.0, false

    /// Break a paragraph into lines using the Knuth-Plass algorithm.
    /// Returns a list of lines with their start/end positions and adjustment ratios.
    /// Raises an exception if no valid breaking is possible.
    ///
    /// This function doesn't mutate `items`.
    let breakLines (options : LineBreakOptions) (items : Item[]) : Line[] =
        if items.Length = 0 then
            [||]
        else
            let n = items.Length
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
                if isValidBreakpoint items i then
                    let penaltyCost, isFlagged = getPenaltyAt items i
                    let isForced = penaltyCost = -infinity

                    // Try nodes at each previous position (only the best per fitness class)
                    for prevPos in max 0 lastForcedBreak .. i - 1 do
                        match computeAdjustmentRatio items sums options.LineWidth prevPos i with
                        | ValueSome ratio when isForced || ratio >= -1.0 ->
                            // Accept if feasible (forced break or ratio >= -1); tolerance is handled in computeDemerits
                            let fitness = fitnessClass ratio
                            let isLast = i = n

                            // Check the best node for each fitness class at this position
                            for fitnessIdx in 0..3 do
                                let prevNodeIdx = getBestNode (enum<FitnessClass> fitnessIdx) prevPos

                                if prevNodeIdx <> Int32.MinValue then
                                    let prevNode = nodes.[prevNodeIdx]

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
            | None ->
                failwithf
                    "No valid line breaking found for paragraph with %d items and line width %.2f. Try: (1) increasing line width, (2) increasing tolerance, or (3) allowing hyphenation"
                    items.Length
                    options.LineWidth
            | Some bestEndIdx ->
                // Backtrack to recover the solution
                let result = ResizeArray ()

                let rec backtrack nodeIdx =
                    let node = nodes.[nodeIdx]

                    match node.PreviousNode with
                    | None ->
                        result.Reverse ()
                        result.ToArray ()
                    | Some prevIdx ->
                        let prevNode = nodes.[prevIdx]

                        let displayRatio =
                            computeDisplayedAdjustmentRatio items sums options.LineWidth prevNode.Position node.Position

                        let line =
                            {
                                Start = prevNode.Position
                                End = node.Position
                                AdjustmentRatio = displayRatio
                            }

                        result.Add line
                        backtrack prevIdx

                backtrack bestEndIdx
