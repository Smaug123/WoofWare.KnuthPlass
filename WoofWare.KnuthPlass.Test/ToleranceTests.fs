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
    let ``Tolerance cannot rescue an infeasible overfull line`` () =
        // Tolerance only affects the demerits assigned to a feasible line. If a break would
        // require shrinking more than the glue allows, it must remain invalid regardless of tolerance.
        let items = [| Items.box 60.0 |]

        // Box is 60 wide, line width is 50
        // This is overfull with no glue to shrink - should always fail even with huge tolerance
        let options =
            { LineBreakOptions.Default 50.0 with
                Tolerance = 5000.0
            }

        Assert.Throws<System.Exception> (fun () -> LineBreaker.breakLines options items |> ignore)
        |> ignore

    [<Test>]
    let ``Underfull lines are always accepted regardless of badness`` () =
        // Underfull lines (ratio >= 0) can always be achieved by stretching.
        // They should be accepted even with very high badness; tolerance just compounds their demerits relative to other plans.

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
        // Badness = 100 * 3^3 = 2700 (way over tolerance)
        // The quadratic penalty makes this line extremely expensive, but it must still be considered because the adjustment is feasible.

        let lines = LineBreaker.breakLines options items
        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldEqual 3.0

    [<Test>]
    let ``Overfull lines within tolerance are accepted`` () =
        // Lines with -1 <= ratio < 0 should be accepted if the shrinkage is feasible.
        // Being within tolerance simply means no extra quadratic penalty is added.

        let items =
            [|
                Items.box 50.0
                Items.glue 10.0 0.0 5.0 // Width 10, can shrink by 5
                Items.box 50.0
            |]

        let options =
            { LineBreakOptions.Default 105.0 with
                Tolerance = 100.0 // Allow badness up to 100
            }

        // Natural width: 50 + 10 + 50 = 110
        // Target: 105
        // Need to shrink by 5, max shrink is 5
        // Ratio = -5/5 = -1.0
        // Badness = 100 * 1^3 = 100 (exactly at tolerance) so the penalty adds nothing.

        let lines = LineBreaker.breakLines options items
        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldEqual -1.0

    [<Test>]
    let ``Higher tolerance allows looser lines`` () =
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
                Tolerance = 0.5
            }

        let looseOptions =
            { LineBreakOptions.Default 100.0 with
                Tolerance = 3.0
            }

        // Both settings should find solutions; the difference is that the stricter tolerance will heavily penalise
        // the looser plan, while the relaxed tolerance lets it compete.
        let strictLines = LineBreaker.breakLines strictOptions items
        let looseLines = LineBreaker.breakLines looseOptions items

        strictLines.Length |> shouldBeGreaterThan 0
        looseLines.Length |> shouldBeGreaterThan 0

    [<Test>]
    let ``Tolerance filtering prunes a globally optimal but tight line`` () =
        // This test verifies that the algorithm correctly chooses the globally optimal solution even when
        // it involves a tight first line (ratio ≈ -0.94, badness ≈ 83). The key issue this guards against
        // is fitness class mismatch penalties dominating the decision: the original test data had a second
        // line with ratio=3.4 (VeryLoose), creating a Tight→VeryLoose mismatch penalty that made the two-line
        // solution worse than one line, even though intuitively two lines should be better.

        let items =
            [|
                Items.box 25.0
                Items.glue 8.0 0.0 8.5
                Items.box 25.0
                Items.penalty 0.0 0.0 false
                Items.glue 15.0 5.0 5.0 // More balanced glue for second line
                Items.box 30.0 // Larger box for second line
            |]

        // With the corrected test data, the globally optimal solution breaks after the penalty:
        // first line contains Box-Glue-Box (ratio ≈ -0.94, Tight fitness, badness ≈ 83) and the second line
        // contains glue and box (ratio = 1.0, Loose fitness, badness = 100). The Tight→Loose fitness mismatch
        // adds only 100 demerits, making the two-line solution clearly better than the one-line alternative
        // which would be extremely overfull.
        let tolerantOptions =
            { LineBreakOptions.Default 50.0 with
                Tolerance = 5000.0
            }

        let optimalLines = LineBreaker.breakLines tolerantOptions items
        optimalLines.Length |> shouldEqual 2
        optimalLines.[0].End |> shouldEqual 4

        (abs (optimalLines.[0].AdjustmentRatio + 0.9411764705882353)) < 1e-6
        |> shouldEqual true

        // Historically the default tolerance incorrectly pruned this tight line, forcing the algorithm to pick a
        // much worse break earlier in the paragraph. This assertion ensures the default options now behave like
        // the tolerant ones above.
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
