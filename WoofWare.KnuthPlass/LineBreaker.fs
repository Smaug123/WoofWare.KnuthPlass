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

[<Struct>]
type internal WidthTriple =
    {
        Width : float
        Stretch : float
        Shrink : float
    }

[<RequireQualifiedAccess>]
module internal WidthTriple =
    let zero : WidthTriple =
        {
            Width = 0.0
            Stretch = 0.0
            Shrink = 0.0
        }

    let inline add (a : WidthTriple) (b : WidthTriple) : WidthTriple =
        {
            Width = a.Width + b.Width
            Stretch = a.Stretch + b.Stretch
            Shrink = a.Shrink + b.Shrink
        }

    let inline subtract (minuend : WidthTriple) (subtrahend : WidthTriple) : WidthTriple =
        {
            Width = minuend.Width - subtrahend.Width
            Stretch = minuend.Stretch - subtrahend.Stretch
            Shrink = minuend.Shrink - subtrahend.Shrink
        }

    let inline ofItem (item : Item) : WidthTriple =
        match item with
        | Box box ->
            {
                Width = box.Width
                Stretch = 0.0
                Shrink = 0.0
            }
        | Glue glue ->
            {
                Width = glue.Width
                Stretch = glue.Stretch
                Shrink = glue.Shrink
            }
        | Penalty _ -> zero

type private ActiveEntryKind =
    | Sentinel
    | ActiveNode of nodeIndex : int
    | Delta of WidthTriple

type private ActiveEntry =
    {
        mutable Prev : int
        mutable Next : int
        mutable Kind : ActiveEntryKind
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
        // Line is too long, need to compress
        else if totalShrink > 0.0 then
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
            Double.NegativeInfinity

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

    type private PendingCandidate =
        {
            PrevNodeIdx : int
            Ratio : float
            Demerits : float
        }

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

            // Track whether there is a forced break at or after each position.
            // This lets us preserve active nodes even when a line is overfull,
            // because TeX keeps at least one candidate alive so a forced break
            // can still terminate the paragraph (yielding an overfull box rather
            // than an outright failure).
            let forcedBreakAhead : bool[] =
                let arr = Array.zeroCreate (n + 1)
                let mutable seen = false

                for idx = n - 1 downto 0 do
                    let isForced =
                        match items.[idx] with
                        | Penalty p when p.Cost = -infinity -> true
                        | _ -> false

                    seen <- seen || isForced
                    arr.[idx] <- seen

                arr

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

            // Active list of candidate predecessors
            let activeEntries = ResizeArray<ActiveEntry> ()

            let addEntry (kind : ActiveEntryKind) =
                let idx = activeEntries.Count

                activeEntries.Add
                    {
                        Prev = idx
                        Next = idx
                        Kind = kind
                    }

                idx

            let insertAfter (existingIdx : int) (newIdx : int) =
                let nextIdx = activeEntries.[existingIdx].Next
                activeEntries.[newIdx].Prev <- existingIdx
                activeEntries.[newIdx].Next <- nextIdx
                activeEntries.[existingIdx].Next <- newIdx
                activeEntries.[nextIdx].Prev <- newIdx

            let removeEntry (idx : int) =
                let prevIdx = activeEntries.[idx].Prev
                let nextIdx = activeEntries.[idx].Next
                activeEntries.[prevIdx].Next <- nextIdx
                activeEntries.[nextIdx].Prev <- prevIdx
                activeEntries.[idx].Prev <- -1
                activeEntries.[idx].Next <- -1

            let activeHead = addEntry ActiveEntryKind.Sentinel
            activeEntries.[activeHead].Prev <- activeHead
            activeEntries.[activeHead].Next <- activeHead

            let nodeActiveEntry = ResizeArray<int> ()

            let ensureNodeEntry (idx : int) =
                while nodeActiveEntry.Count <= idx do
                    nodeActiveEntry.Add -1

            let appendActiveEntryForNode (nodeIdx : int) (nodePos : int) =
                ensureNodeEntry nodeIdx

                let lastActiveIdx = activeEntries.[activeHead].Prev

                let entryIdx =
                    if lastActiveIdx = activeHead then
                        let idx = addEntry (ActiveEntryKind.ActiveNode nodeIdx)
                        insertAfter activeHead idx
                        idx
                    else
                        match activeEntries.[lastActiveIdx].Kind with
                        | ActiveEntryKind.ActiveNode prevNodeIdx ->
                            let prevPos = nodes.[prevNodeIdx].Position

                            let delta =
                                {
                                    Width = sums.Width.[nodePos] - sums.Width.[prevPos]
                                    Stretch = sums.Stretch.[nodePos] - sums.Stretch.[prevPos]
                                    Shrink = sums.Shrink.[nodePos] - sums.Shrink.[prevPos]
                                }

                            let deltaIdx = addEntry (ActiveEntryKind.Delta delta)
                            insertAfter lastActiveIdx deltaIdx

                            let idx = addEntry (ActiveEntryKind.ActiveNode nodeIdx)
                            insertAfter deltaIdx idx
                            idx
                        | _ -> failwith "Invariant violation: last active entry should always be an active node."

                nodeActiveEntry.[nodeIdx] <- entryIdx

            let mutable activeWidth = WidthTriple.zero

            let rec removeActiveEntry (entryIdx : int) =
                if entryIdx = -1 then
                    ()
                else
                    match activeEntries.[entryIdx].Kind with
                    | ActiveEntryKind.ActiveNode nodeIdx ->
                        nodeActiveEntry.[nodeIdx] <- -1
                        let prevIdx = activeEntries.[entryIdx].Prev
                        let nextIdx = activeEntries.[entryIdx].Next

                        match activeEntries.[prevIdx].Kind, activeEntries.[nextIdx].Kind with
                        | ActiveEntryKind.Sentinel, ActiveEntryKind.Sentinel ->
                            activeWidth <- WidthTriple.zero
                            removeEntry entryIdx
                        | ActiveEntryKind.Sentinel, ActiveEntryKind.Delta deltaAfter ->
                            activeWidth <- WidthTriple.subtract activeWidth deltaAfter
                            removeEntry nextIdx
                            removeEntry entryIdx
                        | ActiveEntryKind.Delta deltaBefore, ActiveEntryKind.Delta deltaAfter ->
                            let combined = WidthTriple.add deltaBefore deltaAfter
                            activeEntries.[prevIdx].Kind <- ActiveEntryKind.Delta combined
                            removeEntry nextIdx
                            removeEntry entryIdx
                        | ActiveEntryKind.Delta _, ActiveEntryKind.Sentinel ->
                            removeEntry prevIdx
                            removeEntry entryIdx
                        | (ActiveEntryKind.Sentinel, ActiveEntryKind.ActiveNode _)
                        | (ActiveEntryKind.Delta _, ActiveEntryKind.ActiveNode _) ->
                            // No delta separating entries; simply remove this node.
                            removeEntry entryIdx
                        | ActiveEntryKind.ActiveNode _, _ ->
                            failwith "Invariant violation: active nodes should be separated by delta nodes."
                    | _ -> ()

            let clearActiveList () =
                let mutable entryIdx = activeEntries.[activeHead].Next

                while entryIdx <> activeHead do
                    let nextIdx = activeEntries.[entryIdx].Next

                    match activeEntries.[entryIdx].Kind with
                    | ActiveEntryKind.ActiveNode nodeIdx -> nodeActiveEntry.[nodeIdx] <- -1
                    | _ -> ()

                    activeEntries.[entryIdx].Prev <- -1
                    activeEntries.[entryIdx].Next <- -1
                    entryIdx <- nextIdx

                activeEntries.[activeHead].Next <- activeHead
                activeEntries.[activeHead].Prev <- activeHead
                activeWidth <- WidthTriple.zero

            // Seed the active list with the start node
            appendActiveEntryForNode 0 0

            let inline computeRatioFromTriple (widthTriple : WidthTriple) (endIdx : int) =
                let penaltyWidth =
                    if endIdx > 0 && endIdx <= items.Length then
                        match items.[endIdx - 1] with
                        | Penalty p -> p.Width
                        | _ -> 0.0
                    else
                        0.0

                let actualWidth = widthTriple.Width + penaltyWidth
                let diff = options.LineWidth - actualWidth

                let ratio =
                    if abs diff < 1e-10 then
                        ValueSome 0.0
                    elif diff > 0.0 then
                        if widthTriple.Stretch > 0.0 then
                            ValueSome (diff / widthTriple.Stretch)
                        else
                            ValueSome extremeLooseRatio
                    elif widthTriple.Shrink > 0.0 then
                        ValueSome (diff / widthTriple.Shrink)
                    else
                        ValueNone

                ratio, actualWidth

            // For each position, find the best predecessor
            for i in 1..n do
                // Update the active width to include the contribution from the previous item
                let itemTriple = WidthTriple.ofItem items.[i - 1]
                activeWidth <- WidthTriple.add activeWidth itemTriple

                if isValidBreakpoint items i then
                    let penaltyCost, isFlagged = getPenaltyAt items i
                    let isForced = penaltyCost = -infinity

                    let pendingNodes : PendingCandidate option array = Array.create 4 None
                    let nodesToDeactivate = System.Collections.Generic.HashSet<int> ()
                    let newlyCreatedNodes = ResizeArray<int> ()
                    let mutable rescueCandidate : (int * bool * float * float * float) option = None
                    let mutable anyNodeAdded = false

                    let mutable entryIdx = activeEntries.[activeHead].Next
                    let mutable curActiveWidth = activeWidth

                    while entryIdx <> activeHead do
                        match activeEntries.[entryIdx].Kind with
                        | ActiveEntryKind.Delta delta ->
                            curActiveWidth <- WidthTriple.subtract curActiveWidth delta
                            entryIdx <- activeEntries.[entryIdx].Next
                        | ActiveEntryKind.ActiveNode prevNodeIdx ->
                            let currentEntryIdx = entryIdx
                            entryIdx <- activeEntries.[entryIdx].Next

                            let prevNode = nodes.[prevNodeIdx]
                            let prevPos = prevNode.Position

                            let ratioResult, actualWidth = computeRatioFromTriple curActiveWidth i
                            let overfullAmount = max 0.0 (actualWidth - options.LineWidth)
                            let totalShrinkAvailable = sums.Shrink.[n] - sums.Shrink.[prevPos]
                            let canEverFit = overfullAmount <= totalShrinkAvailable + 1e-9
                            let forcedBreakInTail = forcedBreakAhead.[prevPos]

                            match ratioResult with
                            | ValueSome ratio when isForced || ratio >= -1.0 ->
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

                                let fitnessIdx = int<FitnessClass> fitness

                                let shouldUpdate =
                                    match pendingNodes.[fitnessIdx] with
                                    | None -> true
                                    | Some existing -> demerits < existing.Demerits

                                if shouldUpdate then
                                    pendingNodes.[fitnessIdx] <-
                                        Some
                                            {
                                                PrevNodeIdx = prevNodeIdx
                                                Ratio = ratio
                                                Demerits = demerits
                                            }
                            | _ ->
                                if not canEverFit && not isForced && not forcedBreakInTail then
                                    nodesToDeactivate.Add currentEntryIdx |> ignore
                                else if (isForced || forcedBreakInTail) && overfullAmount > 0.0 then
                                    let overfullRatio =
                                        if curActiveWidth.Shrink > 0.0 then
                                            (options.LineWidth - actualWidth) / curActiveWidth.Shrink
                                        else
                                            Double.NegativeInfinity

                                    let shouldRescue =
                                        match rescueCandidate with
                                        | None -> true
                                        | Some (_, _, existingOverfull, _, existingDemerits) ->
                                            overfullAmount < existingOverfull - 1e-9
                                            || (abs (overfullAmount - existingOverfull) < 1e-9
                                                && prevNode.Demerits < existingDemerits)

                                    if shouldRescue then
                                        rescueCandidate <-
                                            Some (
                                                prevNodeIdx,
                                                isFlagged,
                                                overfullAmount,
                                                overfullRatio,
                                                prevNode.Demerits
                                            )
                        | ActiveEntryKind.Sentinel -> entryIdx <- activeEntries.[entryIdx].Next

                    for entry in nodesToDeactivate do
                        removeActiveEntry entry

                    for fitnessIdx in 0..3 do
                        match pendingNodes.[fitnessIdx] with
                        | Some pending ->
                            let fitness = enum<FitnessClass> fitnessIdx
                            let existingNodeIdx = getBestNode fitness i

                            let shouldAdd =
                                if existingNodeIdx = Int32.MinValue then
                                    true
                                else
                                    pending.Demerits < nodes.[existingNodeIdx].Demerits

                            if shouldAdd then
                                if existingNodeIdx <> Int32.MinValue then
                                    let entry = nodeActiveEntry.[existingNodeIdx]

                                    if entry <> -1 then
                                        removeActiveEntry entry

                                let newNode =
                                    {
                                        Position = i
                                        Demerits = pending.Demerits
                                        Ratio = pending.Ratio
                                        PreviousNode = Some pending.PrevNodeIdx
                                        Fitness = fitness
                                        WasFlagged = isFlagged
                                    }

                                nodes.Add newNode
                                let nodeIdx = nodes.Count - 1
                                setBestNode fitness i nodeIdx
                                ensureNodeEntry nodeIdx
                                nodeActiveEntry.[nodeIdx] <- -1

                                if isForced then
                                    newlyCreatedNodes.Add nodeIdx
                                else
                                    appendActiveEntryForNode nodeIdx i

                                anyNodeAdded <- true
                        | None -> ()

                    if not anyNodeAdded then
                        match rescueCandidate with
                        | Some (prevNodeIdx, flag, _, ratio, prevDemerits) ->
                            let newNode =
                                {
                                    Position = i
                                    Demerits = prevDemerits
                                    Ratio = ratio
                                    PreviousNode = Some prevNodeIdx
                                    Fitness = FitnessClass.Tight
                                    WasFlagged = flag
                                }

                            let fitness = FitnessClass.Tight
                            let existingNodeIdx = getBestNode fitness i

                            let shouldAdd =
                                if existingNodeIdx = Int32.MinValue then
                                    true
                                else
                                    newNode.Demerits < nodes.[existingNodeIdx].Demerits

                            if shouldAdd then
                                if existingNodeIdx <> Int32.MinValue then
                                    let entry = nodeActiveEntry.[existingNodeIdx]

                                    if entry <> -1 then
                                        removeActiveEntry entry

                                nodes.Add newNode
                                let nodeIdx = nodes.Count - 1
                                setBestNode fitness i nodeIdx
                                ensureNodeEntry nodeIdx
                                nodeActiveEntry.[nodeIdx] <- -1

                                if isForced then
                                    newlyCreatedNodes.Add nodeIdx
                                else
                                    appendActiveEntryForNode nodeIdx i

                                anyNodeAdded <- true
                        | None -> ()

                    if isForced then
                        clearActiveList ()

                        for nodeIdx in newlyCreatedNodes do
                            appendActiveEntryForNode nodeIdx i

                        activeWidth <- WidthTriple.zero

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
