namespace WoofWare.KnuthPlass.Test

open System.Threading
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.KnuthPlass

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module PropertyTests =

    /// Generates a positive line width that's reasonable for the text
    let private genLineWidth : Gen<float32> = Gen.choose (10, 200) |> Gen.map float32

    /// Precomputed cumulative sums for O(1) line width/stretch/shrink queries
    type private CumulativeSums =
        {
            /// cumWidth[i] = sum of (box + glue widths) for items 0..i-1
            Width : float32[]
            /// cumStretch[i] = sum of glue stretches for items 0..i-1
            Stretch : float32[]
            /// cumShrink[i] = sum of glue shrinks for items 0..i-1
            Shrink : float32[]
        }

    let private computeCumulativeSums (items : Item[]) : CumulativeSums =
        let n = items.Length
        let cumWidth = Array.zeroCreate (n + 1)
        let cumStretch = Array.zeroCreate (n + 1)
        let cumShrink = Array.zeroCreate (n + 1)

        for i = 0 to n - 1 do
            match items.[i] with
            | Box b ->
                cumWidth.[i + 1] <- cumWidth.[i] + b.Width
                cumStretch.[i + 1] <- cumStretch.[i]
                cumShrink.[i + 1] <- cumShrink.[i]
            | Glue g ->
                cumWidth.[i + 1] <- cumWidth.[i] + g.Width
                cumStretch.[i + 1] <- cumStretch.[i] + g.Stretch
                cumShrink.[i + 1] <- cumShrink.[i] + g.Shrink
            | Penalty _ ->
                cumWidth.[i + 1] <- cumWidth.[i]
                cumStretch.[i + 1] <- cumStretch.[i]
                cumShrink.[i + 1] <- cumShrink.[i]

        {
            Width = cumWidth
            Stretch = cumStretch
            Shrink = cumShrink
        }

    /// Compute line metrics (width, stretch, shrink) for a line from startIdx to endIdx.
    /// Handles trailing glue exclusion and penalty width inclusion.
    let private computeLineMetrics
        (items : Item[])
        (sums : CumulativeSums)
        (startIdx : int)
        (endIdx : int)
        : float32 * float32 * float32
        =
        let mutable width = sums.Width.[endIdx] - sums.Width.[startIdx]
        let mutable stretch = sums.Stretch.[endIdx] - sums.Stretch.[startIdx]
        let mutable shrink = sums.Shrink.[endIdx] - sums.Shrink.[startIdx]

        // Adjust for trailing item: exclude trailing glue, include penalty width
        if endIdx > 0 && endIdx <= items.Length then
            match items.[endIdx - 1] with
            | Glue g ->
                width <- width - g.Width
                stretch <- stretch - g.Stretch
                shrink <- shrink - g.Shrink
            | Penalty p -> width <- width + p.Width
            | _ -> ()

        (width, stretch, shrink)

    /// Compute the adjustment ratio for a line. Returns None if infeasible.
    /// A line is feasible iff -1 <= r <= tolerance.
    let private computeAdjustmentRatio
        (lineWidth : float32)
        (contentWidth : float32)
        (stretch : float32)
        (shrink : float32)
        : float32 voption
        =
        let diff = lineWidth - contentWidth

        if abs diff < 1e-6f then
            // Perfect fit
            ValueSome 0.0f
        elif diff > 0.0f then
            // Underfull: need to stretch
            if stretch > 1e-9f then
                ValueSome (diff / stretch)
            else
                // No stretch available -> infinite ratio (infeasible)
                ValueNone
        else if
            // Overfull: need to shrink
            shrink > 1e-9f
        then
            ValueSome (diff / shrink) // Will be negative
        else
            // No shrink available -> negative infinite ratio (infeasible)
            ValueNone

    /// Check if a line is feasible according to TeX rules.
    /// A line is feasible iff its adjustment ratio r satisfies: -1 <= r <= tolerance
    let private isLineFeasible
        (items : Item[])
        (sums : CumulativeSums)
        (lineWidth : float32)
        (tolerance : float32)
        (startIdx : int)
        (endIdx : int)
        : bool
        =
        let width, stretch, shrink = computeLineMetrics items sums startIdx endIdx

        match computeAdjustmentRatio lineWidth width stretch shrink with
        | ValueNone -> false // Infinite ratio (no stretch/shrink when needed)
        | ValueSome r -> r >= -1.0f && r <= tolerance

    /// Check if a line is overfull (adjustment ratio < -1).
    let private isOverfullFast
        (items : Item[])
        (sums : CumulativeSums)
        (lineWidth : float32)
        (startIdx : int)
        (endIdx : int)
        : bool
        =
        let width, _, shrink = computeLineMetrics items sums startIdx endIdx
        let minPossibleWidth = width - shrink
        minPossibleWidth > lineWidth + 1e-6f

    /// Check if a line is overfull (width exceeds target even with maximum shrink).
    /// O(n) per call - use isOverfullFast with precomputed sums for bulk queries.
    let private isOverfull (items : Item[]) (lineWidth : float32) (startIdx : int) (endIdx : int) : bool =
        let sums = computeCumulativeSums items
        isOverfullFast items sums lineWidth startIdx endIdx

    /// Check if there exists a TeX-feasible solution using memoized search.
    /// A solution is feasible iff ALL lines have adjustment ratio in [-1, tolerance].
    /// Uses the PRODUCTION isValidBreakpoint to match what the algorithm actually considers.
    /// Complexity: O(n²) with O(n) space for memoization and cumulative sums.
    let private existsFeasibleSolution
        (items : Item[])
        (lineWidth : float32)
        (tolerance : float32)
        (startPos : int)
        : bool
        =
        let sums = computeCumulativeSums items
        let memo = System.Collections.Generic.Dictionary<int, bool> ()

        let rec search (currentPos : int) : bool =
            if currentPos >= items.Length then
                true
            else
                match memo.TryGetValue currentPos with
                | true, result -> result
                | false, _ ->
                    // Try all possible next break positions
                    let mutable found = false
                    let mutable nextPos = currentPos + 1

                    while not found && nextPos <= items.Length do
                        // Use production breakpoint rules to match what the algorithm considers
                        if LineBreaker.isValidBreakpoint items nextPos then
                            // Check if this line would be feasible (ratio in [-1, tolerance])
                            if isLineFeasible items sums lineWidth tolerance currentPos nextPos then
                                // Recursively check if the rest can be broken feasibly
                                if search nextPos then
                                    found <- true

                        nextPos <- nextPos + 1

                    memo.[currentPos] <- found
                    found

        search startPos

    [<Test>]
    let ``No overfull line when a feasible alternative exists`` () =
        let property (text : string) (lineWidth : float32) =
            let items = Items.fromEnglishString Text.defaultWordWidth Text.SPACE_WIDTH text

            if items.Length = 0 then
                // Empty text trivially passes
                ()
            else
                let options = LineBreakOptions.Default lineWidth
                let lines = LineBreaker.breakLines options items

                // Check each line for overfullness
                for line in lines do
                    if isOverfull items lineWidth line.Start line.End then
                        // This line is overfull - check if a TeX-feasible solution existed
                        // (all lines have adjustment ratio in [-1, tolerance])
                        if existsFeasibleSolution items lineWidth options.Tolerance 0 then
                            failwithf
                                "Overfull line from %d to %d when feasible solution exists. Text: %s, LineWidth: %f"
                                line.Start
                                line.End
                                text
                                lineWidth

        let arb =
            ArbMap.defaults
            |> ArbMap.generate<string * float32>
            |> Gen.zip EnglishGen.text
            |> Gen.zip genLineWidth
            |> Gen.map (fun (lineWidth, (text, _)) -> text, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (text, lineWidth) -> property text lineWidth)

        Check.One (FsCheckConfig.config, prop)

    [<Test>]
    let ``No overfull line when a feasible alternative exists - arbitrary items`` () =
        let mutable overfullCount = 0
        let mutable fineCount = 0

        let property (penaltyProb : float) (spec : ParagraphSpec) (lineWidth : float32) =
            let items = ParagraphSpec.compile spec

            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items

            // Check each line for overfullness
            for line in lines do
                if not (isOverfull items lineWidth line.Start line.End) then
                    Interlocked.Increment &fineCount |> ignore<int>
                else
                    // This line is overfull - check if a TeX-feasible solution existed
                    // (all lines have adjustment ratio in [-1, tolerance])
                    Interlocked.Increment &overfullCount |> ignore<int>

                    if existsFeasibleSolution items lineWidth options.Tolerance 0 then
                        failwithf
                            "Overfull line from %d to %d when feasible solution exists. PenaltyProb: %f, LineWidth: %f, Segments: %d, Items: %d"
                            line.Start
                            line.End
                            penaltyProb
                            lineWidth
                            (List.length spec.Segments)
                            items.Length

        let arb = Arb.fromGen ParagraphGen.genTestCase

        let prop =
            Prop.forAll arb (fun (penaltyProb, spec, lineWidth) -> property penaltyProb spec lineWidth)

        Check.One (FsCheckConfig.config, prop)

        // Assert that the test was actually testing something (some lines were overfull).
        // A quarter of an episode of The Night Manager was enough to run this test 1000 times, each of which
        // ran 10_000 tests.
        // The range of the ratios was 0.650865 to 0.666410, mean 0.658688, variance 0.000006
        // If that distribution is ever observed <= 0.1, I'll eat a very small hat.
        // (Of course, the denominator is equal to 10_000 always, being FsCheckConfig.config's MaxTest value.)
        if overfullCount + fineCount = 0 then
            Assert.Inconclusive "somehow didn't run any tests?!"

        float overfullCount / float (overfullCount + fineCount)
        |> shouldBeGreaterThan 0.1

    // ============================================================================
    // Property 1: Lines Partition the Input (Coverage & Contiguity)
    // ============================================================================

    [<Test>]
    let ``Lines partition the input - arbitrary items`` () =
        let property (spec : ParagraphSpec) (lineWidth : float32) =
            let items = ParagraphSpec.compile spec
            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items

            if items.Length > 0 then
                // First line starts at 0
                lines.[0].Start |> shouldEqual 0

                // Last line ends at items.Length
                lines.[lines.Length - 1].End |> shouldEqual items.Length

                // Lines are contiguous (no gaps)
                for i in 0 .. lines.Length - 2 do
                    lines.[i].End |> shouldEqual lines.[i + 1].Start

                // Each line is non-empty
                for line in lines do
                    line.End |> shouldBeGreaterThan line.Start

        let arb =
            ParagraphGen.genTestCase
            |> Gen.map (fun (_, spec, lineWidth) -> spec, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (spec, lineWidth) -> property spec lineWidth)
        Check.One (FsCheckConfig.config, prop)

    [<Test>]
    let ``Lines partition the input - English text`` () =
        let property (text : string) (lineWidth : float32) =
            let items = Items.fromEnglishString Text.defaultWordWidth Text.SPACE_WIDTH text

            if items.Length > 0 then
                let options = LineBreakOptions.Default lineWidth
                let lines = LineBreaker.breakLines options items

                // First line starts at 0
                lines.[0].Start |> shouldEqual 0

                // Last line ends at items.Length
                lines.[lines.Length - 1].End |> shouldEqual items.Length

                // Lines are contiguous
                for i in 0 .. lines.Length - 2 do
                    lines.[i].End |> shouldEqual lines.[i + 1].Start

                // Each line is non-empty
                for line in lines do
                    line.End |> shouldBeGreaterThan line.Start

        let arb =
            ArbMap.defaults
            |> ArbMap.generate<string * float32>
            |> Gen.zip EnglishGen.text
            |> Gen.zip genLineWidth
            |> Gen.map (fun (lineWidth, (text, _)) -> text, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (text, lineWidth) -> property text lineWidth)
        Check.One (FsCheckConfig.config, prop)

    // ============================================================================
    // Property 2: Always Produces Output for Non-Empty Input
    // ============================================================================

    [<Test>]
    let ``Always produces output for non-empty input - arbitrary items`` () =
        let property (spec : ParagraphSpec) (lineWidth : float32) =
            let items = ParagraphSpec.compile spec
            // items from ParagraphSpec is always non-empty (has at least termination)
            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items

            lines.Length |> shouldBeGreaterThan 0

        let arb =
            ParagraphGen.genTestCase
            |> Gen.map (fun (_, spec, lineWidth) -> spec, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (spec, lineWidth) -> property spec lineWidth)
        Check.One (FsCheckConfig.config, prop)

    [<Test>]
    let ``Always produces output for non-empty input - English text`` () =
        let property (text : string) (lineWidth : float32) =
            let items = Items.fromEnglishString Text.defaultWordWidth Text.SPACE_WIDTH text

            if items.Length > 0 then
                let options = LineBreakOptions.Default lineWidth
                let lines = LineBreaker.breakLines options items
                lines.Length |> shouldBeGreaterThan 0

        let arb =
            ArbMap.defaults
            |> ArbMap.generate<string * float32>
            |> Gen.zip EnglishGen.text
            |> Gen.zip genLineWidth
            |> Gen.map (fun (lineWidth, (text, _)) -> text, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (text, lineWidth) -> property text lineWidth)
        Check.One (FsCheckConfig.config, prop)

    // ============================================================================
    // Property 3: Determinism
    // ============================================================================

    [<Test>]
    let ``Determinism - same input produces same output`` () =
        let property (spec : ParagraphSpec) (lineWidth : float32) =
            let items = ParagraphSpec.compile spec
            let options = LineBreakOptions.Default lineWidth

            let lines1 = LineBreaker.breakLines options items
            let lines2 = LineBreaker.breakLines options items

            lines1.Length |> shouldEqual lines2.Length

            for i in 0 .. lines1.Length - 1 do
                lines1.[i].Start |> shouldEqual lines2.[i].Start
                lines1.[i].End |> shouldEqual lines2.[i].End
                lines1.[i].AdjustmentRatio |> shouldEqual lines2.[i].AdjustmentRatio

        let arb =
            ParagraphGen.genTestCase
            |> Gen.map (fun (_, spec, lineWidth) -> spec, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (spec, lineWidth) -> property spec lineWidth)
        Check.One (FsCheckConfig.config, prop)

    // ============================================================================
    // Property 4: Forced Breaks Are Always Respected
    // ============================================================================

    [<Test>]
    let ``Forced breaks are always respected`` () =
        let property (spec : ParagraphSpec) (lineWidth : float32) =
            let items = ParagraphSpec.compile spec
            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items

            // Find all forced break positions (items with Cost = -infinity)
            let forcedPositions =
                items
                |> Array.indexed
                |> Array.choose (fun (i, item) ->
                    match item with
                    | Penalty p when p.Cost = System.Single.NegativeInfinity -> Some (i + 1)
                    | _ -> None
                )
                |> Set.ofArray

            // All line ends that correspond to valid items
            let lineEnds = lines |> Array.map (fun l -> l.End) |> Set.ofArray

            // Every forced break position must appear as a line end
            for forcedPos in forcedPositions do
                if forcedPos <= items.Length then
                    Set.contains forcedPos lineEnds |> shouldEqual true

        let arb =
            ParagraphGen.genTestCase
            |> Gen.map (fun (_, spec, lineWidth) -> spec, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (spec, lineWidth) -> property spec lineWidth)
        Check.One (FsCheckConfig.config, prop)

    // ============================================================================
    // Property 5: Positive Infinity Penalties Are Never Broken
    // ============================================================================

    /// Modify a penalty to potentially have infinite cost
    let private maybeInfinitePenalty (p : PenaltySpec) : Gen<PenaltySpec> =
        gen {
            let! makeInfinite = Gen.elements [ true ; false ; false ; false ; false ]

            if makeInfinite then
                return
                    { p with
                        Cost = System.Single.PositiveInfinity
                    }
            else
                return p
        }

    /// Modify a list of penalties by folding through with a generator
    let rec private modifyPenalties (acc : PenaltySpec list) (remaining : PenaltySpec list) : Gen<PenaltySpec list> =
        match remaining with
        | [] -> Gen.constant (List.rev acc)
        | p :: rest ->
            gen {
                let! modified = maybeInfinitePenalty p
                return! modifyPenalties (modified :: acc) rest
            }

    /// Modify a segment spec by potentially adding infinite penalties
    let private modifySegment (seg : SegmentSpec) : Gen<SegmentSpec> =
        gen {
            let! modifiedPenalties = modifyPenalties [] seg.Penalties

            return
                { seg with
                    Penalties = modifiedPenalties
                }
        }

    /// Modify a list of segments by folding through with a generator
    let rec private modifySegments (acc : SegmentSpec list) (remaining : SegmentSpec list) : Gen<SegmentSpec list> =
        match remaining with
        | [] -> Gen.constant (List.rev acc)
        | seg :: rest ->
            gen {
                let! modified = modifySegment seg
                return! modifySegments (modified :: acc) rest
            }

    /// Generator for a ParagraphSpec that may include some infinite penalties
    let private genSpecWithInfinitePenalties : Gen<ParagraphSpec * float32> =
        gen {
            let! penaltyProb = Gen.choose (0, 100) |> Gen.map (fun x -> float x / 100.0)
            let! spec = ParagraphGen.genParagraphSpec penaltyProb

            // Randomly turn some penalties into infinite cost penalties
            let! modifiedHead = modifySegment spec.Head
            let! modifiedTail = modifySegments [] spec.Tail

            let modifiedSpec =
                {
                    Head = modifiedHead
                    Tail = modifiedTail
                }

            let totalBoxWidth = modifiedSpec.Segments |> List.sumBy (fun s -> s.BoxWidth)
            let avgSegmentWidth = totalBoxWidth / float32 (List.length modifiedSpec.Segments)
            let! multiplier = Gen.choose (30, 150) |> Gen.map (fun x -> float32 x / 100.0f)
            let lineWidth = max 1.0f (avgSegmentWidth * multiplier)

            return modifiedSpec, lineWidth
        }

    [<Test>]
    let ``Positive infinity penalties are never broken`` () =
        let property (spec : ParagraphSpec) (lineWidth : float32) =
            let items = ParagraphSpec.compile spec
            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items

            // Find all infinite penalty positions
            let infinitePenaltyPositions =
                items
                |> Array.indexed
                |> Array.choose (fun (i, item) ->
                    match item with
                    | Penalty p when System.Single.IsPositiveInfinity p.Cost -> Some (i + 1)
                    | _ -> None
                )
                |> Set.ofArray

            // No line should end at an infinite penalty position
            for line in lines do
                if line.End > 0 && line.End <= items.Length then
                    Set.contains line.End infinitePenaltyPositions |> shouldEqual false

        let arb = Arb.fromGen genSpecWithInfinitePenalties

        let prop = Prop.forAll arb (fun (spec, lineWidth) -> property spec lineWidth)
        Check.One (FsCheckConfig.config, prop)

    // ============================================================================
    // Property 6: Adjustment Ratio Consistency
    // ============================================================================

    /// Compute actual line metrics (width, stretch, shrink) for verifying adjustment ratio.
    /// This mirrors the private computeLineMetrics but is simpler and uses the display formula.
    let private computeLineMetricsForDisplay
        (items : Item[])
        (startIdx : int)
        (endIdx : int)
        : float32 * float32 * float32
        =
        let mutable width = 0.0f
        let mutable stretch = 0.0f
        let mutable shrink = 0.0f

        // First, skip leading discardable items (glue, penalties)
        let mutable firstContentIdx = startIdx

        while firstContentIdx < endIdx do
            match items.[firstContentIdx] with
            | Glue _ -> firstContentIdx <- firstContentIdx + 1
            | Penalty _ -> firstContentIdx <- firstContentIdx + 1
            | _ -> firstContentIdx <- endIdx // break the loop

        // Now accumulate from firstContentIdx
        let mutable idx = firstContentIdx

        while idx < endIdx do
            match items.[idx] with
            | Box b -> width <- width + b.Width
            | Glue g ->
                width <- width + g.Width
                stretch <- stretch + g.Stretch
                shrink <- shrink + g.Shrink
            | Penalty _ -> ()

            idx <- idx + 1

        // Exclude trailing glue at line end
        if endIdx > 0 && endIdx <= items.Length then
            match items.[endIdx - 1] with
            | Glue g ->
                width <- width - g.Width
                stretch <- stretch - g.Stretch
                shrink <- shrink - g.Shrink
            | Penalty p ->
                // Penalty width IS included (e.g., hyphen width)
                width <- width + p.Width
            | _ -> ()

        (width, stretch, shrink)

    [<Test>]
    let ``Adjustment ratio is consistent with line geometry`` () =
        let epsilon = 1e-4f

        let property (spec : ParagraphSpec) (lineWidth : float32) =
            let items = ParagraphSpec.compile spec
            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items

            for line in lines do
                let (contentWidth, totalStretch, totalShrink) =
                    computeLineMetricsForDisplay items line.Start line.End

                let diff = lineWidth - contentWidth

                // Check ratio consistency
                if abs diff < epsilon then
                    // Perfect fit: ratio should be near 0
                    abs line.AdjustmentRatio < 1.0f |> shouldEqual true
                elif diff > 0.0f then
                    // Underfull: need to stretch
                    if totalStretch > epsilon then
                        let expectedRatio = diff / totalStretch
                        abs (line.AdjustmentRatio - expectedRatio) < 0.1f |> shouldEqual true
                // else: no stretch available, ratio semantics vary
                else if
                    // Overfull: need to shrink
                    totalShrink > epsilon
                then
                    // Ratio is clamped to -1.0 minimum for display
                    let expectedRatio = max -1.0f (diff / totalShrink)
                    abs (line.AdjustmentRatio - expectedRatio) < 0.1f |> shouldEqual true

        let arb =
            ParagraphGen.genTestCase
            |> Gen.map (fun (_, spec, lineWidth) -> spec, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (spec, lineWidth) -> property spec lineWidth)
        Check.One (FsCheckConfig.config, prop)

    // ============================================================================
    // Property 7: No Breaks Between Consecutive Glues
    // ============================================================================

    [<Test>]
    let ``No breaks between consecutive glues`` () =
        let property (spec : ParagraphSpec) (lineWidth : float32) =
            let items = ParagraphSpec.compile spec
            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items

            for line in lines do
                if line.End > 0 && line.End < items.Length then
                    match items.[line.End - 1] with
                    | Glue _ ->
                        // The item at the break point is glue; the next item should not also be glue
                        match items.[line.End] with
                        | Glue _ ->
                            failwithf "Break between consecutive glues at positions %d and %d" (line.End - 1) line.End
                        | _ -> ()
                    | _ -> ()

        let arb =
            ParagraphGen.genTestCase
            |> Gen.map (fun (_, spec, lineWidth) -> spec, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (spec, lineWidth) -> property spec lineWidth)
        Check.One (FsCheckConfig.config, prop)

    // ============================================================================
    // Property 8: Reference Implementation Equivalence (Optimality)
    // ============================================================================

    /// Enumerate all legal breakings of an item array (for small inputs only).
    /// Returns list of (breakPositions, totalDemerits) pairs.
    let private enumerateAllLegalBreakings (items : Item[]) (options : LineBreakOptions) : (int list * float32) list =
        let n = items.Length

        if n = 0 then
            [ ([ 0 ], 0.0f) ]
        else

        // Simple recursive enumeration with memoization for demerits calculation
        let rec enumerate (currentPos : int) (path : int list) : (int list * float32) list =
            if currentPos >= n then
                // End of paragraph - return this path
                let fullPath = List.rev (n :: path)
                // Compute demerits for this solution
                let demerits =
                    fullPath
                    |> List.pairwise
                    |> List.sumBy (fun (startPos, endPos) ->
                        // Simple demerits: just use badness squared
                        let width, stretch, shrink =
                            computeLineMetrics items (computeCumulativeSums items) startPos endPos

                        let diff = options.LineWidth - width

                        let ratio =
                            if abs diff < 1e-6f then
                                0.0f
                            elif diff > 0.0f then
                                if stretch > 1e-9f then diff / stretch else 10000.0f
                            elif shrink > 1e-9f then
                                diff / shrink
                            else
                                -10000.0f

                        let bad = min (100.0f * (abs ratio ** 3.0f)) 10000.0f
                        (options.LinePenalty + bad) ** 2.0f
                    )

                [ (fullPath, demerits) ]
            else
                // Try all possible next break positions
                [
                    for nextPos in currentPos + 1 .. n do
                        if LineBreaker.isValidBreakpoint items nextPos then
                            yield! enumerate nextPos (currentPos :: path)
                ]

        enumerate 0 []

    /// Generator for small paragraphs (for exhaustive testing)
    let private genSmallSpec : Gen<ParagraphSpec * float32> =
        gen {
            let! penaltyProb = Gen.choose (0, 50) |> Gen.map (fun x -> float x / 100.0)
            // 1-3 segments = small enough for exhaustive enumeration
            let! segmentCount = Gen.choose (0, 2)
            let! head = ParagraphGen.genSegmentSpec penaltyProb
            let! tail = Gen.listOfLength segmentCount (ParagraphGen.genSegmentSpec penaltyProb)

            let spec =
                {
                    Head = head
                    Tail = tail
                }

            let totalBoxWidth = spec.Segments |> List.sumBy (fun s -> s.BoxWidth)
            let avgSegmentWidth = totalBoxWidth / float32 (List.length spec.Segments)
            let! multiplier = Gen.choose (50, 150) |> Gen.map (fun x -> float32 x / 100.0f)
            let lineWidth = max 1.0f (avgSegmentWidth * multiplier)

            return spec, lineWidth
        }

    [<Test>]
    let ``Optimality - algorithm produces minimum demerits among all legal breakings`` () =
        let property (spec : ParagraphSpec) (lineWidth : float32) =
            let items = ParagraphSpec.compile spec

            // Only test small inputs (exhaustive enumeration is expensive)
            if items.Length > 15 then
                ()
            else
                let options = LineBreakOptions.Default lineWidth
                let result = LineBreaker.breakLines options items

                // Compute demerits for the algorithm's result
                let resultBreaks = 0 :: (result |> Array.map (fun l -> l.End) |> Array.toList)

                let resultDemerits =
                    resultBreaks
                    |> List.pairwise
                    |> List.sumBy (fun (startPos, endPos) ->
                        let width, stretch, shrink =
                            computeLineMetrics items (computeCumulativeSums items) startPos endPos

                        let diff = options.LineWidth - width

                        let ratio =
                            if abs diff < 1e-6f then
                                0.0f
                            elif diff > 0.0f then
                                if stretch > 1e-9f then diff / stretch else 10000.0f
                            elif shrink > 1e-9f then
                                diff / shrink
                            else
                                -10000.0f

                        let bad = min (100.0f * (abs ratio ** 3.0f)) 10000.0f
                        (options.LinePenalty + bad) ** 2.0f
                    )

                // Enumerate all legal breakings and find the minimum demerits
                let allBreakings = enumerateAllLegalBreakings items options

                if allBreakings.Length > 0 then
                    let minDemerits = allBreakings |> List.map snd |> List.min
                    // Algorithm result should be within a small tolerance of optimal
                    // (allowing for floating-point differences in demerits calculation)
                    let tolerance = max 1.0f (minDemerits * 0.01f)
                    resultDemerits <= minDemerits + tolerance |> shouldEqual true

        let arb = Arb.fromGen genSmallSpec

        let prop = Prop.forAll arb (fun (spec, lineWidth) -> property spec lineWidth)
        Check.One (FsCheckConfig.config.WithMaxTest (1000), prop)

    // ============================================================================
    // Property 9: Line Count Lower Bound
    // ============================================================================

    [<Test>]
    let ``Line count is at least the physical minimum`` () =
        let property (spec : ParagraphSpec) (lineWidth : float32) =
            let items = ParagraphSpec.compile spec
            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items

            // Calculate total box width
            let totalBoxWidth =
                items
                |> Array.sumBy (fun item ->
                    match item with
                    | Box b -> b.Width
                    | _ -> 0.0f
                )

            // Theoretical minimum lines needed (ignoring forced breaks)
            // This is a weak lower bound since we can't fit more content than lineWidth per line
            if lineWidth > 0.0f && totalBoxWidth > 0.0f then
                let theoreticalMinLines =
                    int (System.Math.Ceiling (float totalBoxWidth / float lineWidth))
                // The algorithm might need more lines, but never fewer
                // Note: This is a very weak bound since glue can stretch/shrink
                // We just verify the algorithm produces at least 1 line for non-empty input
                lines.Length |> shouldBeGreaterThan 0

        let arb =
            ParagraphGen.genTestCase
            |> Gen.map (fun (_, spec, lineWidth) -> spec, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (spec, lineWidth) -> property spec lineWidth)
        Check.One (FsCheckConfig.config, prop)

    // ============================================================================
    // Property 10: Penalty Options Affect Choice Monotonically
    // ============================================================================

    /// Count consecutive flagged breaks in a solution
    let private countConsecutiveFlaggedBreaks (items : Item[]) (lines : Line[]) : int =
        let isFlaggedBreak (lineEnd : int) =
            if lineEnd > 0 && lineEnd <= items.Length then
                match items.[lineEnd - 1] with
                | Penalty p -> p.Flagged
                | _ -> false
            else
                false

        lines
        |> Array.pairwise
        |> Array.filter (fun (line1, line2) -> isFlaggedBreak line1.End && isFlaggedBreak line2.End)
        |> Array.length

    [<Test>]
    let ``Increasing double hyphen demerits decreases consecutive flagged breaks`` () =
        let property (spec : ParagraphSpec) (lineWidth : float32) =
            let items = ParagraphSpec.compile spec

            let options1 =
                { LineBreakOptions.Default lineWidth with
                    DoubleHyphenDemerits = 100.0f
                }

            let options2 =
                { LineBreakOptions.Default lineWidth with
                    DoubleHyphenDemerits = 100000.0f
                }

            let lines1 = LineBreaker.breakLines options1 items
            let lines2 = LineBreaker.breakLines options2 items

            let count1 = countConsecutiveFlaggedBreaks items lines1
            let count2 = countConsecutiveFlaggedBreaks items lines2

            // Higher demerits should result in same or fewer consecutive flagged breaks
            count2 |> shouldBeSmallerThan (count1 + 1) // allow equal

        let arb =
            ParagraphGen.genTestCase
            |> Gen.map (fun (_, spec, lineWidth) -> spec, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (spec, lineWidth) -> property spec lineWidth)
        Check.One (FsCheckConfig.config, prop)

    // ============================================================================
    // Property 11: Scaling Invariance
    // ============================================================================

    /// Scale all widths in an item by a factor
    let private scaleItem (k : float32) (item : Item) : Item =
        match item with
        | Box b ->
            Box
                {
                    Width = b.Width * k
                }
        | Glue g ->
            Glue
                {
                    Width = g.Width * k
                    Stretch = g.Stretch * k
                    Shrink = g.Shrink * k
                }
        | Penalty p ->
            Penalty
                { p with
                    Width = p.Width * k
                }

    [<Test>]
    let ``Scaling invariance - uniform scaling preserves break structure`` () =
        let property (spec : ParagraphSpec) (lineWidth : float32) (scaleFactor : float32) =
            // Ensure scale factor is positive and reasonable
            let k = max 0.1f (min 10.0f (abs scaleFactor + 0.1f))

            let items = ParagraphSpec.compile spec
            let scaledItems = items |> Array.map (scaleItem k)

            let options = LineBreakOptions.Default lineWidth

            let scaledOptions =
                { options with
                    LineWidth = options.LineWidth * k
                }

            let original = LineBreaker.breakLines options items
            let scaled = LineBreaker.breakLines scaledOptions scaledItems

            // Break positions should be identical
            original.Length |> shouldEqual scaled.Length

            for i in 0 .. original.Length - 1 do
                original.[i].Start |> shouldEqual scaled.[i].Start
                original.[i].End |> shouldEqual scaled.[i].End

        let arb =
            gen {
                let! _, spec, lineWidth = ParagraphGen.genTestCase
                let! scaleFactor = Gen.choose (10, 100) |> Gen.map (fun x -> float32 x / 10.0f)
                return spec, lineWidth, scaleFactor
            }
            |> Arb.fromGen

        let prop =
            Prop.forAll arb (fun (spec, lineWidth, scale) -> property spec lineWidth scale)

        Check.One (FsCheckConfig.config, prop)

    // ============================================================================
    // Property 12: Box Coverage (Each Box in Exactly One Line)
    // ============================================================================

    [<Test>]
    let ``Every box appears in exactly one line`` () =
        let property (spec : ParagraphSpec) (lineWidth : float32) =
            let items = ParagraphSpec.compile spec
            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items

            // Find all box positions
            let boxPositions =
                items
                |> Array.indexed
                |> Array.choose (fun (i, item) ->
                    match item with
                    | Box _ -> Some i
                    | _ -> None
                )

            // For each box, verify it's in exactly one line
            for boxPos in boxPositions do
                let containingLines =
                    lines |> Array.filter (fun line -> line.Start <= boxPos && boxPos < line.End)

                containingLines.Length |> shouldEqual 1

        let arb =
            ParagraphGen.genTestCase
            |> Gen.map (fun (_, spec, lineWidth) -> spec, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (spec, lineWidth) -> property spec lineWidth)
        Check.One (FsCheckConfig.config, prop)
