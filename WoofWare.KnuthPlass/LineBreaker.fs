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

    /// Special sentinel ratio value indicating that a line has no glue available to stretch.
    /// This signals to badness() that it should return infBad directly (matching TeX's behavior
    /// when s <= 0). We use infinity as a sentinel since it's never a valid adjustment ratio.
    let private noStretchRatio = infinity

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

    /// Compute adjustment ratio for a line from startIdx to endIdx.
    /// This is used during the algorithm to decide which breaks are optimal.
    /// Following TeX's approach, we exclude trailing glue but NOT leading glue here.
    /// Leading glue is only pruned later when displaying/building actual lines.
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

        // Exclude trailing glue: if we're breaking right after a glue at endIdx-1,
        // that glue should not contribute to this line's width (TeX behavior: glue
        // is added to active_width AFTER try_break)
        if endIdx > 0 && endIdx <= itemsArray.Length then
            match itemsArray.[endIdx - 1] with
            | Glue g ->
                actualWidth <- actualWidth - g.Width
                totalStretch <- totalStretch - g.Stretch
                totalShrink <- totalShrink - g.Shrink
            | Penalty p ->
                // Penalty width IS included (e.g., hyphen width)
                actualWidth <- actualWidth + p.Width
            | _ -> ()

        let diff = lineWidth - actualWidth

        if abs diff < 1e-10 then
            ValueSome 0.0
        elif diff > 0.0 then
            // Line is too short, need to stretch
            if totalStretch > 0.0 then
                ValueSome (diff / totalStretch)
            else
                // No glue to stretch - return sentinel that makes badness = inf_bad
                // (matches TeX's behavior when s <= 0 in tex.web:16110)
                ValueSome noStretchRatio
        // Line is too long, need to compress
        else if totalShrink > 0.0 then
            ValueSome (diff / totalShrink)
        else
            // No glue to shrink and line is overfull - cannot fit
            ValueNone

    /// Once a layout has been chosen we display the per-line ratio as TeX would perceive it,
    /// which means discarding any trailing glue at the breakpoint and leading glue at the start.
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

        // Exclude trailing glue
        if endIdx > 0 && endIdx <= itemsArray.Length then
            match itemsArray.[endIdx - 1] with
            | Glue g ->
                actualWidth <- actualWidth - g.Width
                totalStretch <- totalStretch - g.Stretch
                totalShrink <- totalShrink - g.Shrink
            | Penalty p -> actualWidth <- actualWidth + p.Width
            | _ -> ()

        // Exclude leading discardable items
        let mutable idx = startIdx

        while idx < endIdx && idx < itemsArray.Length do
            match itemsArray.[idx] with
            | Glue g ->
                actualWidth <- actualWidth - g.Width
                totalStretch <- totalStretch - g.Stretch
                totalShrink <- totalShrink - g.Shrink
                idx <- idx + 1
            | Penalty _ -> idx <- idx + 1
            | _ -> idx <- endIdx

        let diff = lineWidth - actualWidth

        if abs diff < 1e-10 then
            0.0
        elif diff > 0.0 then
            if totalStretch > 0.0 then
                diff / totalStretch
            else
                noStretchRatio
        // Clamp overfull lines to ratio -1.0.
        // Note: TeX's hpack (tex.web:13104-13115) sets glue_sign to "normal" (not "shrinking")
        // when there's no shrink available, so TeX doesn't actually produce a meaningful -1.0
        // ratio in that case. However, we need to return a usable value for our API, and -1.0
        // is a sensible convention meaning "maximally compressed / overfull".
        else if totalShrink > 0.0 then
            max -1.0 (diff / totalShrink)
        else
            // No shrink available but line is overfull: return -1.0 as our convention
            // for "maximally compressed". This provides a usable value rather than NaN/infinity.
            -1.0

    let inline private fitnessClass (ratio : float) : FitnessClass =
        if ratio < -0.5 then FitnessClass.Tight
        elif ratio <= 0.5 then FitnessClass.Normal
        elif ratio <= 1.0 then FitnessClass.Loose
        else FitnessClass.VeryLoose

    let inline private badness (ratio : float) : float =
        // Following TeX's badness function (tex.web:16108-16118):
        // - If no stretch/shrink available (signaled by noStretchRatio sentinel): return inf_bad
        // - If ratio too extreme: cap at inf_bad
        // - Otherwise: compute as 100 * ratio³
        if ratio = noStretchRatio || ratio = -noStretchRatio then
            LineBreakOptions.infBad
        else
            let r = abs ratio
            min (100.0 * (r ** 3.0)) LineBreakOptions.infBad

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
        // TeX formula (tex.web:16901): d := line_penalty + badness
        let linePenalty = options.LinePenalty + bad

        // Compute base demerits according to TeX formula (tex.web:16901-16905):
        // - d := (line_penalty + badness)²
        // - if penalty > 0 then d := d + penalty²
        // - else if penalty > eject_penalty then d := d - penalty² (encourages breaking)
        // - Forced breaks (penalty = eject_penalty = -10000): no penalty term added
        let mutable demerits =
            let baseDemerits = linePenalty * linePenalty

            if penaltyCost = -infinity then
                // Forced break (eject_penalty): no penalty term
                baseDemerits
            elif penaltyCost >= 0.0 then
                // Positive penalty: add penalty² (tex.web:16904)
                baseDemerits + (penaltyCost * penaltyCost)
            else
                // Negative penalty reduces demerits (tex.web:16905)
                baseDemerits - (penaltyCost * penaltyCost)

        // Penalty for consecutive flagged breaks (double hyphen)
        if prevWasFlagged && currIsFlagged then
            demerits <- demerits + options.DoubleHyphenDemerits

        // Penalty for fitness class mismatch (tex.web:16909)
        // TeX only adds adj_demerits when |fit_class - fitness(r)| > 1
        // Adjacent classes (diff = 1) do NOT incur a penalty in TeX
        let fitnessDiff = abs (int prevFitness - int currFitness)

        if fitnessDiff > 1 then
            demerits <- demerits + options.AdjacentLooseTightDemerits

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
        if idx >= itemsArray.Length then
            // End of paragraph - implicit forced break (TeX behavior)
            -infinity, false
        elif idx > 0 then
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

            // Track whether there is an explicit forced break at or after each position.
            // This lets us preserve active nodes when an upcoming -infinity penalty could
            // rescue an overfull line. The implicit paragraph end is handled separately
            // (arr.[n] is set to true so the final line is still treated as forced).
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

                arr.[n] <- true // Implicit end-of-paragraph is a forced break
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
                // Start with the raw width from cumulative sums
                let mutable adjustedWidth = widthTriple.Width
                let mutable adjustedStretch = widthTriple.Stretch
                let mutable adjustedShrink = widthTriple.Shrink

                // Exclude trailing glue: if we're breaking right after a glue at endIdx-1,
                // that glue should not contribute to this line's width (TeX behavior: glue
                // is added to active_width AFTER try_break is called)
                //
                // Note: We do NOT exclude leading glue here. That only happens later when
                // displaying/building actual lines (in post_line_break). During the algorithm,
                // TeX uses the full cumulative width to decide which breaks are optimal.
                if endIdx > 0 && endIdx <= items.Length then
                    match items.[endIdx - 1] with
                    | Glue g ->
                        adjustedWidth <- adjustedWidth - g.Width
                        adjustedStretch <- adjustedStretch - g.Stretch
                        adjustedShrink <- adjustedShrink - g.Shrink
                    | Penalty p ->
                        // Penalty width IS included (e.g., hyphen width)
                        adjustedWidth <- adjustedWidth + p.Width
                    | _ -> ()

                let diff = options.LineWidth - adjustedWidth

                let ratio =
                    if abs diff < 1e-10 then
                        ValueSome 0.0
                    elif diff > 0.0 then
                        if adjustedStretch > 0.0 then
                            ValueSome (diff / adjustedStretch)
                        else
                            ValueSome noStretchRatio
                    elif adjustedShrink > 0.0 then
                        ValueSome (diff / adjustedShrink)
                    else
                        ValueNone

                ratio, adjustedWidth, adjustedShrink

            // For each position, find the best predecessor
            let pendingNodes : PendingCandidate option array = Array.create 4 None
            let nodesToDeactivate = System.Collections.Generic.HashSet<int> ()
            let newlyCreatedNodes = ResizeArray<int> ()
            // Nodes that can't ever fit (overfull even with all remaining shrink) when there's
            // no explicit forced break ahead. We revisit them only at the final forced break.
            let deferredForFinalBreak = System.Collections.Generic.HashSet<int> ()

            for i in 1..n do
                newlyCreatedNodes.Clear ()
                nodesToDeactivate.Clear ()

                for j = 0 to pendingNodes.Length - 1 do
                    pendingNodes.[j] <- None

                // Update the active width to include the contribution from the previous item
                let itemTriple = WidthTriple.ofItem items.[i - 1]
                activeWidth <- WidthTriple.add activeWidth itemTriple

                if isValidBreakpoint items i then
                    let penaltyCost, isFlagged = getPenaltyAt items i
                    let isForced = penaltyCost = -infinity
                    // Check if this is an explicit forced break (penalty item) vs implicit paragraph end
                    let isExplicitForcedBreak =
                        isForced
                        && i > 0
                        && i <= items.Length
                        && match items.[i - 1] with
                           | Penalty p -> p.Cost = -infinity
                           | _ -> false

                    // TeX's final-pass rescue (tex.web:16815-16831): at the implicit paragraph end,
                    // we MUST produce output even if no feasible solution exists. Overfull/underfull
                    // boxes are acceptable as a last resort. This is equivalent to TeX's behavior
                    // where it "dare not lose all active nodes" during the final pass.
                    let isImplicitParagraphEnd = i = n

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

                            let ratioResult, actualWidth, _ = computeRatioFromTriple curActiveWidth i

                            let overfullAmount = max 0.0 (actualWidth - options.LineWidth)
                            let totalShrinkAvailable = sums.Shrink.[n] - sums.Shrink.[prevPos]
                            let canEverFit = overfullAmount <= totalShrinkAvailable + 1e-9
                            let forcedBreakInTail = forcedBreakAhead.[prevPos]

                            match ratioResult with
                            | ValueSome ratio when isForced || (ratio >= -1.0 && badness ratio <= options.Tolerance) ->
                                // TeX feasibility check: accept if forced (explicit or implicit) OR (not overfull AND badness within tolerance)
                                // Both explicit forced breaks and implicit paragraph end bypass tolerance to avoid paragraph failures
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
                            | ValueNone ->
                                // No shrink available (after adjusting for trailing glue).
                                // TeX's final-pass rescue (tex.web:16815-16831): at EXPLICIT forced breaks
                                // OR the implicit paragraph end, we create overfull boxes rather than fail.
                                // This ensures paragraphs always produce output.

                                if (isExplicitForcedBreak || isImplicitParagraphEnd) && overfullAmount > 0.0 then
                                    // Forced break with no shrink. TeX creates an overfull box with ratio = -infinity.
                                    let overfullRatio = -infinity

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
                                elif not canEverFit && not isForced && not forcedBreakInTail then
                                    nodesToDeactivate.Add currentEntryIdx |> ignore
                                    deferredForFinalBreak.Add prevNodeIdx |> ignore
                            | ValueSome ratio ->
                                // Ratio doesn't meet feasibility conditions, but might be rescuable
                                if not canEverFit && not isForced && not forcedBreakInTail then
                                    nodesToDeactivate.Add currentEntryIdx |> ignore
                                    deferredForFinalBreak.Add prevNodeIdx |> ignore
                                // Create rescue candidate for overfull lines (ratio < 0) when there's a forced break
                                // (explicit or implicit paragraph end). Having ValueSome (vs ValueNone) means
                                // there's some shrink available but it's not enough.
                                // For underfull lines (ratio >= 0), forced breaks are handled by the feasibility check above.
                                else if
                                    (isExplicitForcedBreak || isImplicitParagraphEnd || forcedBreakInTail)
                                    && ratio < 0.0
                                then
                                    let overfullRatio = ratio

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

                    if isImplicitParagraphEnd && deferredForFinalBreak.Count > 0 then
                        for deferredNodeIdx in deferredForFinalBreak do
                            let prevNode = nodes.[deferredNodeIdx]
                            let prevPos = prevNode.Position

                            let widthTriple =
                                {
                                    Width = sums.Width.[i] - sums.Width.[prevPos]
                                    Stretch = sums.Stretch.[i] - sums.Stretch.[prevPos]
                                    Shrink = sums.Shrink.[i] - sums.Shrink.[prevPos]
                                }

                            let ratioResult, actualWidth, _ = computeRatioFromTriple widthTriple i
                            let overfullAmount = max 0.0 (actualWidth - options.LineWidth)

                            match ratioResult with
                            | ValueSome ratio ->
                                let fitness = fitnessClass ratio
                                let fitnessIdx = int<FitnessClass> fitness
                                let isLast = true

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

                                let shouldUpdate =
                                    match pendingNodes.[fitnessIdx] with
                                    | None -> true
                                    | Some existing -> demerits < existing.Demerits

                                if shouldUpdate then
                                    pendingNodes.[fitnessIdx] <-
                                        Some
                                            {
                                                PrevNodeIdx = deferredNodeIdx
                                                Ratio = ratio
                                                Demerits = demerits
                                            }

                                anyNodeAdded <- true
                            | ValueNone ->
                                // Forced break with no shrink: rescue with an overfull box.
                                if overfullAmount > 0.0 then
                                    let overfullRatio = -infinity

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
                                                deferredNodeIdx,
                                                isFlagged,
                                                overfullAmount,
                                                overfullRatio,
                                                prevNode.Demerits
                                            )

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
