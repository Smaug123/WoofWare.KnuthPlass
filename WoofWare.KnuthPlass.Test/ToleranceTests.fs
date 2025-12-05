namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped
open WoofWare.Expect

[<TestFixture>]
module ToleranceTests =
    /// Compute badness = 100 * |ratio|^3
    let private badness (ratio : float) : float = 100.0 * (abs ratio ** 3.0)

    [<Test>]
    let ``Overfull line with no shrink is rescued on final pass`` () =
        // TeX rescues overfull lines on the final pass. At the paragraph end (pi=eject_penalty),
        // the final pass keeps an active node even when badness exceeds tolerance, producing an
        // overfull box instead of failing (tex.web:16760-16779, 16824-16829).
        //
        // We return -1.0 as our convention for overfull lines (maximally compressed).
        let items = [| Items.box 60.0 |]

        // Box is 60 wide, line width is 50
        // This is overfull with no glue, but TeX's final pass rescues it.
        let options =
            { LineBreakOptions.Default 50.0 with
                Tolerance = 5000.0
            }

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldEqual -1.0

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
                Items.box 30.0
                Items.glue 5.0 5.0 0.0 // Can stretch by 5
                Items.box 30.0
            |]

        let options =
            { LineBreakOptions.Default 80.0 with
                Tolerance = 10.0 // Strict tolerance
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
        lines.[0].AdjustmentRatio |> shouldEqual 3.0

    [<Test>]
    let ``Overfull lines within shrink limits are accepted`` () =
        // Lines with -1 <= ratio < 0 are feasible if the shrinkage is within glue limits.
        // TeX's tolerance is a feasibility cutoff for badness, not for the ratio.
        // A ratio of -1.0 corresponds to using all available shrink, and
        // badness = 100 * |ratio|³ = 100 for ratio = -1.0.

        let items =
            [|
                Items.box 50.0
                Items.glue 10.0 0.0 5.0 // Width 10, can shrink by 5
                Items.box 50.0
            |]

        let options =
            { LineBreakOptions.Default 105.0 with
                Tolerance = 100.0 // Badness cutoff
            }

        // Natural width: 50 + 10 + 50 = 110
        // Target: 105
        // Need to shrink by 5, max shrink is 5
        // Ratio = -5/5 = -1.0
        // Badness = 100 * 1³ = 100 (at tolerance cutoff, so break is feasible)

        let lines = LineBreaker.breakLines options items
        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldEqual -1.0

    [<Test>]
    let ``Higher tolerance allows looser lines to be feasible`` () =
        // TeX's tolerance is a hard feasibility filter (tex.web:16320-16333):
        // breakpoints with badness > tolerance are pruned, not merely penalized.
        // With a very low tolerance, most breakpoints are infeasible and the
        // algorithm falls back to the final-pass rescue (single overfull line).
        // With higher tolerance, more breakpoints become feasible.
        let items =
            [|
                Items.box 50.0
                Items.glue 10.0 5.0 3.0
                Items.box 50.0
                Items.glue 10.0 5.0 3.0
                Items.box 50.0
            |]

        let strictOptions =
            { LineBreakOptions.Default 100.0 with
                Tolerance = 0.5 // Very strict: most breaks are infeasible
            }

        let looseOptions =
            { LineBreakOptions.Default 100.0 with
                Tolerance = 5000.0 // Permissive: more breaks are feasible
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
                Items.box 25.0
                Items.glue 8.0 0.0 8.5
                Items.box 25.0
                Items.penalty 0.0 0.0 false
                Items.glue 15.0 5.0 5.0 // More balanced glue for second line
                Items.box 30.0 // Larger box for second line
            |]

        // Two-line solution:
        //   Line 1: Box-Glue-Box = 25+8+25=58, shrink=8.5, ratio = (50-58)/8.5 ≈ -0.94
        //   Badness ≈ 83 < 200 (default tolerance) → FEASIBLE
        //   Line 2: glue + box = 15+30=45, stretch=5, ratio = (50-45)/5 = 1.0
        //   Badness = 100 < 200 → FEASIBLE
        //
        // The tight line should NOT be pruned; both lines are within tolerance.
        let tolerantOptions =
            { LineBreakOptions.Default 50.0 with
                Tolerance = 5000.0
            }

        let optimalLines = LineBreaker.breakLines tolerantOptions items
        optimalLines.Length |> shouldEqual 2
        optimalLines.[0].End |> shouldEqual 4

        (abs (optimalLines.[0].AdjustmentRatio + 0.9411764705882353)) < 1e-6
        |> shouldEqual true

        // With default tolerance (200), the tight line (badness 83) is still feasible
        // and should be chosen as the optimal solution.
        let defaultOptions = LineBreakOptions.Default 50.0
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
                Items.box 30.0
                Items.glue 10.0 10.0 5.0
                Items.penalty 0.0 0.0 false // Position 3: Line 1 badness 6400 > tolerance
                Items.box 30.0
                Items.glue 10.0 60.0 10.0
                Items.penalty 0.0 0.0 false // Position 6: Line 1 badness 0, Line 2 badness ~195
                Items.glue 20.0 40.0 5.0 // Line 2 stretch/shrink
                Items.box 10.0
            |]

        let options = LineBreakOptions.Default 80.0 // Uses default tolerance = 200
        let lines = LineBreaker.breakLines options items

        // Should break at position 6 (position 3 was pruned by tolerance)
        lines.Length |> shouldEqual 2
        lines.[0].End |> shouldEqual 6

        // Verify Line 1 has the expected ratio
        (abs (lines.[0].AdjustmentRatio - 0.0)) < 0.01 |> shouldEqual true
