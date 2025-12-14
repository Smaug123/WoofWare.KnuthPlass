namespace WoofWare.KnuthPlass

open System

[<Struct>]
type private BreakNode =
    {
        Position : int
        Demerits : float32
        Ratio : float32
        /// -1 for a sentinel None
        PreviousNode : int
        Fitness : FitnessClass
        WasFlagged : bool
    }

/// Precomputed cumulative sums for efficient line computation
type private CumulativeSums =
    {
        Width : float32[]
        Stretch : float32[]
        Shrink : float32[]
    }

[<Struct>]
type internal WidthTriple =
    {
        Width : float32
        Stretch : float32
        Shrink : float32
    }

[<RequireQualifiedAccess>]
module internal WidthTriple =
    let zero : WidthTriple =
        {
            Width = 0.0f
            Stretch = 0.0f
            Shrink = 0.0f
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
                Stretch = 0.0f
                Shrink = 0.0f
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

/// Precomputed state that is shared between passes.
/// This is computed once at the start of breakLines and reused for both passes.
type private PrecomputedState =
    {
        Sums : CumulativeSums
        WidthMinusShrink : float32[]
        SuffixMinWidthMinusShrink : float32[]
        ForcedBreakAhead : bool[]
    }

/// The module holding the heart of the Knuth-Plass algorithm.
[<RequireQualifiedAccess>]
module LineBreaker =

#if DEBUG
    /// Debug tracing controlled by WOOFWARE_KNUTH_PLASS_DEBUG environment variable.
    /// Set to "1" or "true" to enable tracing to stderr.
    /// Only available in DEBUG builds.
    let private debugEnabled =
        match Environment.GetEnvironmentVariable "WOOFWARE_KNUTH_PLASS_DEBUG" with
        | null -> false
        | s -> s = "1" || String.Equals (s, "true", StringComparison.OrdinalIgnoreCase)

    let inline private trace (msgFn : unit -> string) =
        if debugEnabled then
            Console.Error.WriteLine (msgFn ())
#else
    let inline private trace (_msgFn : unit -> string) = ()
#endif

    /// Special sentinel ratio value indicating that a line has no glue available to stretch.
    /// This signals to badness() that it should return infBad directly (matching TeX's behavior
    /// when s <= 0). We use infinity as a sentinel since it's never a valid adjustment ratio.
    let private noStretchRatio = Single.PositiveInfinity

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

    let private computePrecomputedState (items : Item[]) : PrecomputedState =
        let n = items.Length
        let sums = computeCumulativeSums items

        // Precompute width-minus-shrink to cheaply bound how small a line can ever be
        // if we keep extending it. This lets us drop hopeless active nodes early.
        let widthMinusShrink = Array.zeroCreate (n + 1)

        for i = 0 to n do
            widthMinusShrink.[i] <- sums.Width.[i] - sums.Shrink.[i]

        let suffixMinWidthMinusShrink = Array.zeroCreate (n + 1)
        let mutable runningMin = Single.PositiveInfinity

        for i = n downto 0 do
            let candidate = widthMinusShrink.[i]
            runningMin <- min runningMin candidate
            suffixMinWidthMinusShrink.[i] <- runningMin

        // Track whether there is an EXPLICIT forced break at or after each position.
        // This lets us preserve active nodes when an upcoming -infinity penalty could
        // rescue an overfull line. The implicit paragraph end is NOT counted here
        // (only arr.[n] is true) - the rescue logic at forced breaks handles that case.
        // This distinction is important for performance: if we counted the implicit end,
        // forcedBreakInTail would be true for ALL positions, preventing node deactivation.
        let forcedBreakAhead : bool[] =
            let arr = Array.zeroCreate (n + 1)
            arr.[n] <- true // Implicit end-of-paragraph is a forced break
            let mutable seen = false // Only track explicit forced breaks in the loop

            for idx = n - 1 downto 0 do
                let isForced =
                    match items.[idx] with
                    | Penalty p when p.Cost = Single.NegativeInfinity -> true
                    | _ -> false

                seen <- seen || isForced
                arr.[idx] <- seen

            arr

        {
            Sums = sums
            WidthMinusShrink = widthMinusShrink
            SuffixMinWidthMinusShrink = suffixMinWidthMinusShrink
            ForcedBreakAhead = forcedBreakAhead
        }

    /// Compute adjustment ratio for a line from startIdx to endIdx.
    /// This is used during the algorithm to decide which breaks are optimal.
    /// Following TeX's approach, we exclude trailing glue but NOT leading glue here.
    /// Leading glue is only pruned later when displaying/building actual lines.
    let private computeAdjustmentRatio
        (itemsArray : Item array)
        (sums : CumulativeSums)
        (lineWidth : float32)
        (startIdx : int)
        (endIdx : int)
        : float32 voption
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

        if abs diff < 1e-10f then
            ValueSome 0.0f
        elif diff > 0.0f then
            // Line is too short, need to stretch
            if totalStretch > 0.0f then
                ValueSome (diff / totalStretch)
            else
                // No glue to stretch - return sentinel that makes badness = inf_bad
                // (matches TeX's behavior when s <= 0 in tex.web:16110)
                ValueSome noStretchRatio
        // Line is too long, need to compress
        else if totalShrink > 0.0f then
            ValueSome (diff / totalShrink)
        else
            // No glue to shrink and line is overfull - cannot fit
            ValueNone

    /// Once a layout has been chosen we display the per-line ratio as TeX would perceive it,
    /// which means discarding any trailing glue at the breakpoint and leading glue at the start.
    let private computeDisplayedAdjustmentRatio
        (itemsArray : Item array)
        (sums : CumulativeSums)
        (lineWidth : float32)
        (startIdx : int)
        (endIdx : int)
        : float32
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

        if abs diff < 1e-10f then
            0.0f
        elif diff > 0.0f then
            if totalStretch > 0.0f then
                diff / totalStretch
            else
                noStretchRatio
        // Clamp overfull lines to ratio -1.0.
        // Note: TeX's hpack (tex.web:13104-13115) sets glue_sign to "normal" (not "shrinking")
        // when there's no shrink available, so TeX doesn't actually produce a meaningful -1.0
        // ratio in that case. However, we need to return a usable value for our API, and -1.0
        // is a sensible convention meaning "maximally compressed / overfull".
        else if totalShrink > 0.0f then
            max -1.0f (diff / totalShrink)
        else
            // No shrink available but line is overfull: return -1.0 as our convention
            // for "maximally compressed". This provides a usable value rather than NaN/infinity.
            -1.0f

    let inline private fitnessClass (ratio : float32) : FitnessClass =
        if ratio < -0.5f then FitnessClass.Tight
        elif ratio <= 0.5f then FitnessClass.Normal
        elif ratio <= 1.0f then FitnessClass.Loose
        else FitnessClass.VeryLoose

    let inline private badness (ratio : float32) : float32 =
        // Following TeX's badness function (tex.web:16108-16118):
        // - If no stretch/shrink available (signaled by noStretchRatio sentinel): return inf_bad
        // - If ratio too extreme: cap at inf_bad
        // - Otherwise: compute as 100 * ratio³
        if ratio = noStretchRatio || ratio = -noStretchRatio then
            LineBreakOptions.infBad
        else
            let r = abs ratio
            min (100.0f * (r ** 3.0f)) LineBreakOptions.infBad

    let private computeDemerits
        (options : LineBreakOptions)
        (ratio : float32)
        (penaltyCost : float32)
        (prevFitness : FitnessClass)
        (currFitness : FitnessClass)
        (prevWasFlagged : bool)
        (currIsFlagged : bool)
        (isLastLine : bool)
        : float32
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

            if penaltyCost = Single.NegativeInfinity then
                // Forced break (eject_penalty): no penalty term
                baseDemerits
            elif penaltyCost >= 0.0f then
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

    let private getPenaltyAt (itemsArray : Item array) (idx : int) : float32 * bool =
        if idx >= itemsArray.Length then
            // End of paragraph - implicit forced break (TeX behavior)
            Single.NegativeInfinity, false
        elif idx > 0 then
            match itemsArray.[idx - 1] with
            | Penalty p -> p.Cost, p.Flagged
            | _ -> 0.0f, false
        else
            0.0f, false

    type private PendingCandidate =
        {
            PrevNodeIdx : int
            Ratio : float32
            Demerits : float32
        }

    /// Try to break a paragraph into lines.
    /// Returns ValueSome lines on success, ValueNone if no valid breaking is possible.
    ///
    /// This implements TeX's line-breaking algorithm with a two-pass structure:
    /// - Pass 1 (isFinalPass=false): Normal tolerance check, deactivate nodes that can't fit
    /// - Pass 2 (isFinalPass=true): Same tolerance, but with "artificial demerits" guard
    ///
    /// The artificial demerits guard (tex.web:16815-16831) prevents losing the last active node
    /// on the final pass. When about to deactivate the ONLY remaining active node, it instead:
    /// - Suppresses the deactivation (keeps the node active)
    /// - Records a break with demerits = 0 in the pending slot
    ///
    /// This ensures the algorithm always produces output on the final pass, even if it
    /// requires overfull or severely underfull lines.
    ///
    /// Note on hyphenation: TeX has a pre-tolerance pass that runs without hyphenation,
    /// only enabling it if that fails. Since hyphenation is handled externally to this
    /// algorithm (items already include hyphenation points), we effectively always have
    /// hyphenation enabled. This means we may hyphenate in cases where TeX wouldn't.
    let private tryBreakLines
        (options : LineBreakOptions)
        (isFinalPass : bool)
        (items : Item[])
        (precomputed : PrecomputedState)
        : Line[] voption
        =
        trace (fun () -> $"=== Starting tryBreakLines: isFinalPass=%b{isFinalPass} ===")
        let n = items.Length
        let sums = precomputed.Sums
        let widthMinusShrink = precomputed.WidthMinusShrink
        let suffixMinWidthMinusShrink = precomputed.SuffixMinWidthMinusShrink
        let forcedBreakAhead = precomputed.ForcedBreakAhead

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
                Demerits = 0.0f
                Ratio = 0.0f
                PreviousNode = -1
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
                    | ActiveEntryKind.Sentinel, ActiveEntryKind.ActiveNode _
                    | ActiveEntryKind.Delta _, ActiveEntryKind.ActiveNode _ ->
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

        /// Check if an entry is the only ActiveNode in the active list (not counting delta entries).
        /// This is used for TeX's artificial demerits guard (tex.web:16815-16831).
        let isOnlyActiveNode (entryIdx : int) : bool =
            let inline isDelta idx =
                match activeEntries.[idx].Kind with
                | Delta _ -> true
                | _ -> false

            // Walk backwards from entryIdx, skipping Delta entries
            let mutable prev = activeEntries.[entryIdx].Prev

            while prev <> activeHead && isDelta prev do
                prev <- activeEntries.[prev].Prev

            // Walk forwards from entryIdx, skipping Delta entries
            let mutable next = activeEntries.[entryIdx].Next

            while next <> activeHead && isDelta next do
                next <- activeEntries.[next].Next

            // It's the only active node if prev is sentinel and next is sentinel
            prev = activeHead && next = activeHead

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
                if abs diff < 1e-10f then
                    ValueSome 0.0f
                elif diff > 0.0f then
                    if adjustedStretch > 0.0f then
                        ValueSome (diff / adjustedStretch)
                    else
                        ValueSome noStretchRatio
                elif adjustedShrink > 0.0f then
                    ValueSome (diff / adjustedShrink)
                else
                    ValueNone

            ratio, adjustedWidth, adjustedShrink

        // For each position, find the best predecessor
        let pendingNodes : PendingCandidate option array = Array.create 4 None
        let nodesToDeactivate = System.Collections.Generic.HashSet<int> ()
        let newlyCreatedNodes = ResizeArray<int> ()

        // On pass 2, track high-badness but non-overfull breaks as rescue candidates.
        // If no feasible break is found, we use the best rescue candidate.
        // This ensures non-overfull paths aren't completely lost due to tolerance filtering.
        let mutable rescueCandidate : PendingCandidate voption = ValueNone

        for i in 1..n do
            newlyCreatedNodes.Clear ()
            nodesToDeactivate.Clear ()
            rescueCandidate <- ValueNone

            for j = 0 to pendingNodes.Length - 1 do
                pendingNodes.[j] <- None

            // Update the active width to include the contribution from the previous item
            let itemTriple = WidthTriple.ofItem items.[i - 1]
            activeWidth <- WidthTriple.add activeWidth itemTriple

            if isValidBreakpoint items i then
                let penaltyCost, isFlagged = getPenaltyAt items i
                let isForced = penaltyCost = Single.NegativeInfinity

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

                        let minPossibleWidth = suffixMinWidthMinusShrink.[i] - widthMinusShrink.[prevPos]

                        let forcedBreakInTail = forcedBreakAhead.[prevPos]
                        let noFutureFit = minPossibleWidth > options.LineWidth + 1e-9f

                        trace (fun () ->
                            let ratioStr =
                                match ratioResult with
                                | ValueSome r -> $"%.4f{r}"
                                | ValueNone -> "None"

                            $"  Considering break %d{prevPos}->%d{i}: ratio=%s{ratioStr}, width=%.2f{actualWidth}, isForced=%b{isForced}, forcedBreakInTail=%b{forcedBreakInTail}"
                        )

                        match ratioResult with
                        // Use small epsilon for ratio >= -1 check to handle floating-point precision issues.
                        // Cumulative sums can accumulate small errors (e.g., shrink=0.9999998808 instead of 1.0),
                        // causing ratio to be -1.0000001 instead of -1.0, which would incorrectly fail the check.
                        //
                        // Feasibility check: require ratio >= -1 AND badness <= tolerance.
                        // This applies on BOTH passes to maintain O(N) complexity.
                        // The artificial_demerits guard (elsewhere) handles rescue when needed.
                        | ValueSome ratio when ratio >= -1.0f - 1e-6f && badness ratio <= options.Tolerance ->
                            // Feasibility: accept if non-overfull AND within tolerance
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

                            trace (fun () ->
                                $"    FEASIBLE: ratio=%.4f{ratio}, badness=%.2f{badness ratio}, demerits=%.2f{demerits}, fitness=%d{int fitness}, shouldUpdate=%b{shouldUpdate}"
                            )

                            if shouldUpdate then
                                pendingNodes.[fitnessIdx] <-
                                    Some
                                        {
                                            PrevNodeIdx = prevNodeIdx
                                            Ratio = ratio
                                            Demerits = demerits
                                        }
                        | ValueNone ->
                            // No shrink available (line is overfull and cannot shrink).
                            // This is always infeasible - we don't bypass for forced breaks.
                            // The artificial_demerits guard handles rescue when needed.
                            trace (fun () -> "    INFEASIBLE (ValueNone): no shrink available")

                            if noFutureFit && not forcedBreakInTail then
                                // This node can never fit and there's no forced break ahead where it
                                // might be rescued. Consider deactivating it.
                                // TeX's artificial demerits guard (tex.web:16815-16831):
                                // On the final pass, when about to deactivate the ONLY remaining active node,
                                // suppress the deactivation and record a break with demerits = 0.
                                // This ensures at least one path survives to the paragraph end.
                                if isFinalPass && isOnlyActiveNode currentEntryIdx then
                                    trace (fun () ->
                                        "    ARTIFICIAL DEMERITS (ValueNone): suppressing deactivation, recording d=0"
                                    )
                                    // Use ratio = -infinity for no-shrink case, fitness = Tight
                                    let artificialRatio = Single.NegativeInfinity
                                    let fitness = FitnessClass.Tight
                                    let fitnessIdx = int<FitnessClass> fitness

                                    let shouldUpdate =
                                        match pendingNodes.[fitnessIdx] with
                                        | None -> true
                                        | Some existing -> 0.0f < existing.Demerits

                                    if shouldUpdate then
                                        pendingNodes.[fitnessIdx] <-
                                            Some
                                                {
                                                    PrevNodeIdx = prevNodeIdx
                                                    Ratio = artificialRatio
                                                    Demerits = 0.0f
                                                }
                                // DON'T deactivate - keep the node active
                                else
                                    nodesToDeactivate.Add currentEntryIdx |> ignore
                        // else: noFutureFit is false or forcedBreakInTail is true - keep the node active
                        | ValueSome ratio ->
                            // Ratio doesn't meet feasibility conditions (overfull or badness > tolerance)
                            let isNonOverfull = ratio >= -1.0f - 1e-6f

                            trace (fun () ->
                                $"    INFEASIBLE: ratio=%.4f{ratio} (>= -1: %b{isNonOverfull}), badness=%.2f{badness ratio} (<= tol: %b{badness ratio <= options.Tolerance}), noFutureFit=%b{noFutureFit}, forcedBreakInTail=%b{forcedBreakInTail}"
                            )

                            // On pass 2, if this break is non-overfull but high-badness, track it as a rescue candidate.
                            // We'll use it if no feasible break is found at this position.
                            // Include noStretchRatio breaks - even severely underfull lines are better than
                            // overfull lines according to the feasibility definition (minWidth <= lineWidth).
                            if isFinalPass && isNonOverfull then
                                let fitness = fitnessClass ratio
                                let isLast = i = n

                                let candidateDemerits =
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

                                let isBetter =
                                    match rescueCandidate with
                                    | ValueNone -> true
                                    | ValueSome existing -> candidateDemerits < existing.Demerits

                                if isBetter then
                                    rescueCandidate <-
                                        ValueSome
                                            {
                                                PrevNodeIdx = prevNodeIdx
                                                Ratio = ratio
                                                Demerits = candidateDemerits
                                            }

                            // Consider deactivating if the line can never fit and there's no forced break ahead
                            if noFutureFit && not isForced && not forcedBreakInTail then
                                // TeX's artificial demerits guard (tex.web:16815-16831):
                                // On the final pass, when about to deactivate the ONLY remaining active node,
                                // suppress the deactivation and record a break with demerits = 0.
                                // This ensures at least one path survives even when all breaks are infeasible.
                                if isFinalPass && isOnlyActiveNode currentEntryIdx then
                                    trace (fun () -> $"    ARTIFICIAL DEMERITS: recording d=0 with ratio=%.4f{ratio}")

                                    let fitness = fitnessClass ratio
                                    let fitnessIdx = int<FitnessClass> fitness

                                    let shouldUpdate =
                                        match pendingNodes.[fitnessIdx] with
                                        | None -> true
                                        | Some existing -> 0.0f < existing.Demerits

                                    if shouldUpdate then
                                        pendingNodes.[fitnessIdx] <-
                                            Some
                                                {
                                                    PrevNodeIdx = prevNodeIdx
                                                    Ratio = ratio
                                                    Demerits = 0.0f
                                                }
                                // DON'T deactivate - keep the node active
                                else
                                    nodesToDeactivate.Add currentEntryIdx |> ignore
                    // else: node is kept active (could fit later, or there's a forced break ahead)
                    | ActiveEntryKind.Sentinel -> entryIdx <- activeEntries.[entryIdx].Next

                // On pass 2, if no feasible break was found but we have a non-overfull rescue candidate,
                // use it. This allows the algorithm to explore paths through high-badness breaks,
                // which may ultimately lead to non-overfull solutions.
                if isFinalPass then
                    let anyFeasibleFound =
                        pendingNodes
                        |> Array.exists (
                            function
                            | Some _ -> true
                            | None -> false
                        )

                    if not anyFeasibleFound then
                        match rescueCandidate with
                        | ValueSome candidate ->
                            trace (fun () ->
                                $"    RESCUE CANDIDATE: using high-badness non-overfull break with ratio=%.4f{candidate.Ratio}, demerits=%.2f{candidate.Demerits}"
                            )

                            let fitness = fitnessClass candidate.Ratio
                            let fitnessIdx = int<FitnessClass> fitness
                            pendingNodes.[fitnessIdx] <- Some candidate
                        | ValueNone -> ()

                // TeX-style rescue for forced breaks (tex.web:16815-16831):
                // If we're at a forced break on the final pass and NO feasible path was found,
                // we MUST create a rescue break. Otherwise the algorithm would fail.
                // This must run BEFORE deactivations so we can find active nodes to rescue from.
                if isFinalPass && isForced then
                    let anyFeasibleFound =
                        pendingNodes
                        |> Array.exists (
                            function
                            | Some _ -> true
                            | None -> false
                        )

                    if not anyFeasibleFound then
                        trace (fun () -> "    FORCED BREAK RESCUE: no feasible path found, creating rescue break")

                        // Find the active node with lowest accumulated demerits.
                        // This node provides the "best" infeasible path to complete the paragraph.
                        let mutable bestNodeIdx = -1
                        let mutable bestDemerits = Single.PositiveInfinity
                        let mutable rescueEntry = activeEntries.[activeHead].Next

                        while rescueEntry <> activeHead do
                            match activeEntries.[rescueEntry].Kind with
                            | ActiveEntryKind.ActiveNode nodeIdx ->
                                if nodes.[nodeIdx].Demerits < bestDemerits then
                                    bestDemerits <- nodes.[nodeIdx].Demerits
                                    bestNodeIdx <- nodeIdx

                                rescueEntry <- activeEntries.[rescueEntry].Next
                            | _ -> rescueEntry <- activeEntries.[rescueEntry].Next

                        if bestNodeIdx <> -1 then
                            let fitness = FitnessClass.Tight
                            let fitnessIdx = int<FitnessClass> fitness

                            trace (fun () ->
                                $"    RESCUE: creating break from node %d{bestNodeIdx} with demerits %.2f{bestDemerits}"
                            )

                            pendingNodes.[fitnessIdx] <-
                                Some
                                    {
                                        PrevNodeIdx = bestNodeIdx
                                        Ratio = Single.NegativeInfinity
                                        Demerits = bestDemerits
                                    }

                // Now perform deactivations (after forced break rescue has found active nodes)
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
                                    PreviousNode = pending.PrevNodeIdx
                                    Fitness = fitness
                                    WasFlagged = isFlagged
                                }

                            trace (fun () ->
                                $"  ADDING NODE at %d{i}: from %d{pending.PrevNodeIdx}, ratio=%.4f{pending.Ratio}, demerits=%.2f{pending.Demerits}, fitness=%d{int fitness}"
                            )

                            nodes.Add newNode
                            let nodeIdx = nodes.Count - 1
                            setBestNode fitness i nodeIdx
                            ensureNodeEntry nodeIdx
                            nodeActiveEntry.[nodeIdx] <- -1

                            if isForced then
                                newlyCreatedNodes.Add nodeIdx
                            else
                                appendActiveEntryForNode nodeIdx i
                    | None -> ()

                if isForced then
                    clearActiveList ()

                    for nodeIdx in newlyCreatedNodes do
                        appendActiveEntryForNode nodeIdx i

                    activeWidth <- WidthTriple.zero

        // Find best ending node
        let mutable bestEndIdx = -1
        let mutable minV = Single.PositiveInfinity

        for i = 0 to nodes.Count - 1 do
            let node = nodes.[i]

            if node.Position = n then
                if node.Demerits < minV then
                    minV <- node.Demerits
                    bestEndIdx <- i

        // If no node reached the paragraph end, this pass failed
        if bestEndIdx = -1 then
            ValueNone
        else
            // Backtrack to recover the solution
            let result = ResizeArray ()

            let rec backtrack nodeIdx =
                let node = nodes.[nodeIdx]

                match node.PreviousNode with
                | -1 ->
                    result.Reverse ()
                    result.ToArray ()
                | prevIdx ->
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

            ValueSome (backtrack bestEndIdx)

    /// Break a paragraph into lines using the Knuth-Plass algorithm.
    /// Returns a list of lines with their start/end positions and adjustment ratios.
    ///
    /// This implements a two-pass algorithm following TeX's approach:
    /// - Pass 1: Normal tolerance check, may fail if no feasible solution exists
    /// - Pass 2 (final pass): Same tolerance, but with "artificial demerits" guard
    ///   that prevents losing the last active node, ensuring output is always produced
    ///
    /// Note on hyphenation: TeX has a pre-tolerance pass that runs without hyphenation,
    /// only enabling it if that fails. Since hyphenation is handled externally to this
    /// algorithm (items already include hyphenation points), we effectively always have
    /// hyphenation enabled. This means we may hyphenate in cases where TeX wouldn't.
    ///
    /// This function doesn't mutate `items`.
    let breakLines (options : LineBreakOptions) (items : Item[]) : Line[] =
        if items.Length = 0 then
            [||]
        else
            let precomputed = computePrecomputedState items

            // Pass 1: Normal tolerance, no artificial_demerits guard
            match tryBreakLines options false items precomputed with
            | ValueSome lines -> lines
            | ValueNone ->
                // Pass 2: Final pass with artificial_demerits guard
                match tryBreakLines options true items precomputed with
                | ValueSome lines -> lines
                | ValueNone ->
                    // This should be impossible - the final pass should always produce output
                    // because the artificial_demerits guard prevents losing all active nodes
                    failwith "Impossible: final pass should always produce output"
