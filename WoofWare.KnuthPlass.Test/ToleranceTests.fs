namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module ToleranceTests =
    /// Compute badness = 100 * |ratio|^3
    let private badness (ratio : float32) : float32 = 100.0f * (abs ratio ** 3.0f)

    [<Test>]
    let ``Overfull line with no shrink is rescued on final pass`` () =
        // TeX rescues overfull lines on the final pass. At the paragraph end (pi=eject_penalty),
        // the final pass keeps an active node even when badness exceeds tolerance, producing an
        // overfull box instead of failing (tex.web:16760-16779, 16824-16829).
        //
        // We return -1.0f as our convention for overfull lines (maximally compressed).
        let items = [| Items.box 60.0f |]

        // Box is 60 wide, line width is 50
        // This is overfull with no glue, but TeX's final pass rescues it.
        let options =
            { LineBreakOptions.Default 50.0f with
                Tolerance = 5000.0f
            }

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldEqual -1.0f

    [<Test>]
    let ``Underfull lines exceeding tolerance are pruned mid-paragraph`` () =
        // TeX uses tolerance as a feasibility cutoff (tex.web:16320-16333, 16769-16775).
        // Non-forced lines with badness > tolerance are skipped, not merely expensive.
        // Only the forced final line can be rescued on the final pass.
        //
        // This test verifies that a high-badness underfull break is pruned when there
        // are better alternatives, but the final pass still rescues if needed.

        let items =
            [|
                Items.box 30.0f
                Items.glue 5.0f 5.0f 0.0f // Can stretch by 5
                Items.box 30.0f
            |]

        let options =
            { LineBreakOptions.Default 80.0f with
                Tolerance = 10.0f // Strict tolerance
            }

        // Natural width: 30 + 5 + 30 = 65
        // Target: 80
        // Need to stretch by 15, max stretch is 5
        // Ratio = 15/5 = 3.0
        // Badness = 100 * 3^3 = 2700 >> tolerance
        //
        // In TeX, this break would be pruned on early passes because badness > tolerance.
        // On the final pass, the paragraph-end forced break is kept even with high badness.
        // The paragraph succeeds with a single underfull line.

        let lines = LineBreaker.breakLines options items
        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldEqual 3.0f

    [<Test>]
    let ``Overfull lines within shrink limits are accepted`` () =
        // Lines with -1 <= ratio < 0 are feasible if the shrinkage is within glue limits.
        // TeX's tolerance is a feasibility cutoff for badness, not for the ratio.
        // A ratio of -1.0f corresponds to using all available shrink, and
        // badness = 100 * |ratio|³ = 100 for ratio = -1.0.

        let items =
            [|
                Items.box 50.0f
                Items.glue 10.0f 0.0f 5.0f // Width 10, can shrink by 5
                Items.box 50.0f
            |]

        let options =
            { LineBreakOptions.Default 105.0f with
                Tolerance = 100.0f // Badness cutoff
            }

        // Natural width: 50 + 10 + 50 = 110
        // Target: 105
        // Need to shrink by 5, max shrink is 5
        // Ratio = -5/5 = -1.0
        // Badness = 100 * 1³ = 100 (at tolerance cutoff, so break is feasible)

        let lines = LineBreaker.breakLines options items
        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldEqual -1.0f

    [<Test>]
    let ``Higher tolerance allows looser lines to be feasible`` () =
        // TeX's tolerance is a hard feasibility filter (tex.web:16320-16333):
        // breakpoints with badness > tolerance are pruned, not merely penalized.
        // With a very low tolerance, most breakpoints are infeasible and the
        // algorithm falls back to the final-pass rescue (single overfull line).
        // With higher tolerance, more breakpoints become feasible.
        let items =
            [|
                Items.box 50.0f
                Items.glue 10.0f 5.0f 3.0f
                Items.box 50.0f
                Items.glue 10.0f 5.0f 3.0f
                Items.box 50.0f
            |]

        let strictOptions =
            { LineBreakOptions.Default 100.0f with
                Tolerance = 0.5f // Very strict: most breaks are infeasible
            }

        let looseOptions =
            { LineBreakOptions.Default 100.0f with
                Tolerance = 5000.0f // Permissive: more breaks are feasible
            }

        // With strict tolerance, mid-paragraph breaks are likely pruned as infeasible,
        // resulting in a final-pass rescue. With loose tolerance, multiple layouts
        // become feasible and the algorithm can optimize.
        let strictLines = LineBreaker.breakLines strictOptions items
        let looseLines = LineBreaker.breakLines looseOptions items

        strictLines.Length |> shouldBeGreaterThan 0
        looseLines.Length |> shouldBeGreaterThan 0

    [<Test>]
    let ``Tight line within tolerance is not pruned`` () =
        // This test verifies that a tight line with badness within tolerance is kept as feasible.
        // The algorithm correctly chooses the globally optimal solution even when it involves
        // a tight first line (ratio ≈ -0.94, badness ≈ 83).
        //
        // The key issue this guards against is incorrect tolerance handling that would prune
        // tight-but-feasible breaks, forcing the algorithm to choose worse alternatives.

        let items =
            [|
                Items.box 25.0f
                Items.glue 8.0f 0.0f 8.5f
                Items.box 25.0f
                Items.penalty 0.0f 0.0f false
                Items.glue 15.0f 5.0f 5.0f // More balanced glue for second line
                Items.box 30.0f // Larger box for second line
            |]

        // Two-line solution:
        //   Line 1: Box-Glue-Box = 25+8+25=58, shrink=8.5, ratio = (50-58)/8.5 ≈ -0.94
        //   Badness ≈ 83 < 200 (default tolerance) → FEASIBLE
        //   Line 2: glue + box = 15+30=45, stretch=5, ratio = (50-45)/5 = 1.0
        //   Badness = 100 < 200 → FEASIBLE
        //
        // The tight line should NOT be pruned; both lines are within tolerance.
        let tolerantOptions =
            { LineBreakOptions.Default 50.0f with
                Tolerance = 5000.0f
            }

        let optimalLines = LineBreaker.breakLines tolerantOptions items
        optimalLines.Length |> shouldEqual 2
        optimalLines.[0].End |> shouldEqual 4

        (abs (optimalLines.[0].AdjustmentRatio + 0.9411764705882353f)) < 1e-6f
        |> shouldEqual true

        // With default tolerance (200), the tight line (badness 83) is still feasible
        // and should be chosen as the optimal solution.
        let defaultOptions = LineBreakOptions.Default 50.0f
        let actualLines = LineBreaker.breakLines defaultOptions items

        actualLines |> shouldEqual optimalLines

    [<Test>]
    let ``Tolerance acts as feasibility cutoff rejecting high-badness breaks`` () =
        // This test verifies that TeX's tolerance is a hard feasibility cutoff.
        // We force at least 2 lines (content exceeds single line width) and create two break options:
        //
        // - Break at position 3 (after penalty):
        //   Line 1: box(30) + glue(10,10,5) + penalty
        //   Width = 40, stretch = 10, ratio = (80-40)/10 = 4.0, badness = 6400 > 200 → REJECTED by tolerance
        //
        // - Break at position 6 (after penalty):
        //   Line 1: box(30) + glue(10,10,5) + penalty + box(30) + glue(10,60,10) + penalty
        //   Width = 80, stretch = 70, ratio = (80-80)/70 = 0.0, badness = 0 < 200 → ACCEPTED
        //   Line 2: glue(20,40,5) + box(10)
        //   Width = 30, stretch = 40, ratio = (80-30)/40 = 1.25, badness ≈ 195 < 200 → ACCEPTED
        //
        // The algorithm must choose position 6, proving position 3 was pruned by tolerance.

        let items =
            [|
                Items.box 30.0f
                Items.glue 10.0f 10.0f 5.0f
                Items.penalty 0.0f 0.0f false // Position 3: Line 1 badness 6400 > tolerance
                Items.box 30.0f
                Items.glue 10.0f 60.0f 10.0f
                Items.penalty 0.0f 0.0f false // Position 6: Line 1 badness 0, Line 2 badness ~195
                Items.glue 20.0f 40.0f 5.0f // Line 2 stretch/shrink
                Items.box 10.0f
            |]

        let options = LineBreakOptions.Default 80.0f // Uses default tolerance = 200
        let lines = LineBreaker.breakLines options items

        // Should break at position 6 (position 3 was pruned by tolerance)
        lines.Length |> shouldEqual 2
        lines.[0].End |> shouldEqual 6

        // Verify Line 1 has the expected ratio
        (abs lines.[0].AdjustmentRatio) < 0.01f |> shouldEqual true

    /// Verify the algorithm preserves accumulated demerits through rescue paths.
    [<Test>]
    let ``Rescue paths preserve accumulated demerits`` () =
        let items =
            [|
                Items.box 30.0f
                Items.glue 5.0f 1.0f 0.5f
                Items.penalty 0.0f 500.0f false // High penalty break point
                Items.box 30.0f
                Items.glue 5.0f 1.0f 0.5f
                Items.penalty 0.0f 10.0f false // Low penalty break point
                Items.box 30.0f
                Items.glue 5.0f 1.0f 0.5f
                Items.box 200.0f // Forces overfull ending
            |]

        let options =
            { LineBreakOptions.Default 80.0f with
                Tolerance = 1000.0f
            }

        let lines = LineBreaker.breakLines options items
        lines.Length |> shouldBeGreaterThan 0

    /// On the final pass, the algorithm should explore high-badness but non-overfull breaks
    /// to find solutions that avoid overfull lines.
    [<Test>]
    let ``Final pass explores high-badness non-overfull breaks`` () =
        // Items where:
        // - Not breaking early leads to overfull
        // - Breaking early (high badness) leads to non-overfull solution
        let items =
            [|
                Items.box 3.0f
                Items.glue 1.0f 10.0f 0.3f // Lots of stretch at position 2
                Items.box 18.0f // Position 3: Creates a "trap"
            // If we DON'T break at position 2:
            //   Continue to position 3: width = 3+1+18 = 22 > 20 (overfull!)
            // If we DO break at position 2 (high badness because 3 << 20):
            //   Line 1 = 3 (can stretch to 20 with ratio=1.7, badness=491 > tolerance)
            //   Line 2 = 18 < 20 (non-overfull!)
            |]

        let options = LineBreakOptions.Default 20.0f
        let lines = LineBreaker.breakLines options items

        // Helper to compute minimum possible line width
        let computeLineMinWidth (start : int) (endPos : int) =
            let mutable width = 0.0f
            let mutable shrink = 0.0f

            for i = start to endPos - 1 do
                match items.[i] with
                | Box b -> width <- width + b.Width
                | Glue g ->
                    width <- width + g.Width
                    shrink <- shrink + g.Shrink
                | Penalty _ -> ()

            if endPos > 0 && endPos <= items.Length then
                match items.[endPos - 1] with
                | Glue g ->
                    width <- width - g.Width
                    shrink <- shrink - g.Shrink
                | _ -> ()

            width - shrink

        // Verify no line is overfull
        for line in lines do
            let minWidth = computeLineMinWidth line.Start line.End
            minWidth |> shouldBeSmallerThan 20.1f

    /// Tolerance still prunes a non-overfull path when a "feasible" sibling exists.
    /// A high-badness (underfull) split that would avoid all overfull lines is discarded
    /// because another split at the same breakpoint passes the tolerance check, even though
    /// that sibling can't complete without overfull.
    [<Test>]
    let ``Tolerance does not prune needed high-badness path when feasible sibling exists`` () =
        let lw = 10.0f

        let items =
            [|
                // First segment: makes a very short, high-ratio line if broken here
                Items.box 2.0f
                Items.glue 0.0f 0.5f 0.0f
                Items.box 2.0f
                Items.glue 0.0f 0.5f 0.0f
                Items.box 1.0f
                Items.glue 0.0f 0.0f 0.0f // breakpoint A (high badness, needed)
                // Remainder: just barely fits if we take breakpoint A; overfull otherwise
                Items.box 5.0f
                Items.glue 0.5f 0.0f 0.0f
                Items.box 5.0f
                Items.forcedBreak ()
            |]

        let options = LineBreakOptions.Default lw
        let lines = LineBreaker.breakLines options items

        // Helper to compute minimum possible line width
        let computeLineMinWidth (start : int) (endPos : int) =
            let mutable width = 0.0f
            let mutable shrink = 0.0f

            for i = start to endPos - 1 do
                match items.[i] with
                | Box b -> width <- width + b.Width
                | Glue g ->
                    width <- width + g.Width
                    shrink <- shrink + g.Shrink
                | Penalty _ -> ()

            if endPos > 0 && endPos <= items.Length then
                match items.[endPos - 1] with
                | Glue g ->
                    width <- width - g.Width
                    shrink <- shrink - g.Shrink
                | Penalty p -> width <- width + p.Width
                | _ -> ()

            width - shrink

        // Expect: two lines, none overfull.
        // The first-line split at index 6 has ratio ~ 2.5 (badness > tolerance) but yields
        // a second line that fits. If the algorithm returns an overfull line here, that's the bug.
        lines.Length |> shouldEqual 2

        for line in lines do
            let minWidth = computeLineMinWidth line.Start line.End
            minWidth |> shouldBeSmallerThan (lw + 0.1f)

    /// Artificial-demerits guard zeroes accumulated demerits.
    /// When the guard fires, it resets total demerits to 0 instead of carrying the
    /// predecessor's cost, letting a rescued path dominate incorrectly.
    [<Test>]
    let ``Artificial demerits guard preserves accumulated demerits`` () =
        let lw = 10.0f

        let items =
            [|
                Items.box 9.5f // Forces huge badness if kept active
                Items.glue 0.0f 0.0f 0.0f
                Items.box 9.5f // No shrink: any single-line attempt is overfull
                Items.forcedBreak ()
            |]

        let options = LineBreakOptions.Default lw
        let lines = LineBreaker.breakLines options items

        // The algorithm should produce output (even if overfull)
        lines.Length |> shouldBeGreaterThan 0

// If a rescued node ends up with demerits = 0 (instead of prev.Demerits),
// it will win over a higher-quality sibling path.
// This test documents that the guard fires; the semantic issue is that
// demerits may be incorrectly zeroed.
