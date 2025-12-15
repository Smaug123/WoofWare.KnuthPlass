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
        float overfullCount / float (overfullCount + fineCount)
        |> shouldBeGreaterThan 0.1
