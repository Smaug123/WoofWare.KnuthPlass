namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module LineBreakingTests =
    [<Test>]
    let ``Long paragraph breaks into multiple lines with sufficient tolerance`` () =
        // With LineBreakOptions.Default (tolerance=200), TeX considers no breakpoint feasible
        // before the paragraph end: breaks at early positions have badness > tolerance.
        // TeX's final pass would keep only the paragraph-end forced break, yielding a single
        // overfull line. To actually get multiple lines, we need higher tolerance.
        let items =
            [|
                Items.box 30.0f
                Items.glue 10.0f 5.0f 3.0f
                Items.box 25.0f
                Items.glue 10.0f 5.0f 3.0f
                Items.box 30.0f
                Items.glue 10.0f 5.0f 3.0f
                Items.box 35.0f
                Items.glue 10.0f 5.0f 3.0f
                Items.box 20.0f
            |]

        // Raise tolerance to allow looser lines to be feasible
        let options =
            { LineBreakOptions.Default 80.0f with
                Tolerance = 5000.0f
            }

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldBeGreaterThan 1

        // Check that lines don't overlap
        for i in 0 .. lines.Length - 2 do
            (lines.[i].End <= lines.[i + 1].Start) |> shouldEqual true

    [<Test>]
    let ``Breaking prefers balanced lines over greedy breaks`` () =
        // This tests the key feature of Knuth-Plass: it finds globally optimal breaks.
        //
        // We have 6 boxes of width 8, separated by 5 glues of width 2 (stretch 1, shrink 1).
        // Total natural width = 6×8 + 5×2 = 58. With line width 30, we need 2 lines.
        //
        // Balanced break after item 5 (3 boxes + 2 glues):
        //   Line 1: 3×8 + 2×2 = 28, stretch = 2, ratio = (30-28)/2 = 1.0
        //   Line 2: 3×8 + 2×2 = 28, stretch = 2, ratio = (30-28)/2 = 1.0
        //
        // A greedy algorithm might try to fit 4 boxes on line 1:
        //   Line 1: 4×8 + 3×2 = 38, shrink = 3, ratio = (30-38)/3 = -2.67 (overfull!)
        //
        // Knuth-Plass correctly finds the balanced solution with lower total demerits.

        let items =
            [|
                Items.box 8.0f
                Items.glue 2.0f 1.0f 1.0f // width/stretch/shrink
                Items.box 8.0f
                Items.glue 2.0f 1.0f 1.0f
                Items.box 8.0f
                Items.glue 2.0f 1.0f 1.0f
                Items.box 8.0f
                Items.glue 2.0f 1.0f 1.0f
                Items.box 8.0f
                Items.glue 2.0f 1.0f 1.0f
                Items.box 8.0f
            |]

        let options = LineBreakOptions.Default 30.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 2

        // The algorithm should prefer balanced lines (both around ratio 1.0)
        for line in lines do
            (abs line.AdjustmentRatio < 2.0f) |> shouldEqual true

        // Both lines should have similar adjustment ratios
        let ratios = lines |> Array.map (fun l -> l.AdjustmentRatio)
        let maxRatio = ratios |> Array.max
        let minRatio = ratios |> Array.min
        (maxRatio - minRatio < 1.0f) |> shouldEqual true

    [<Test>]
    let ``Underfull line without glue is infeasible`` () =
        // An underfull line with no glue to stretch is infeasible: there is no way to
        // adjust the glue to fill the line width. TeX would assign ratio = infinity (inf_bad)
        // making such a break infeasible under normal tolerance (tex.web:2337-2342).
        //
        // Scenario: Three boxes that could break in two ways:
        // 1. Break after "word1 word2", leaving just "word3" alone on second line
        //    - Line 2 would be just Box(40) = 40 units on a 70-unit line with NO glue
        //    - Shortfall = 30, stretch = 0, ratio = infinity → INFEASIBLE
        // 2. Keep "word1 word2 word3" together on one line
        //    - Total: 15 + 10 + 15 + 10 + 40 = 90 units on 70-unit line
        //    - Need to shrink by 20, total shrink = 20
        //    - Ratio = -20/20 = -1.0, badness = 100 → FEASIBLE
        //
        // Option 1 is rejected because the second line is infeasible (infinite badness),
        // not because of high demerits. The algorithm chooses option 2.
        let items =
            [|
                Items.box 15.0f // "word1"
                Items.glue 10.0f 5.0f 10.0f // Need shrink=10 to make one-line solution achievable
                Items.box 15.0f // "word2"
                Items.glue 10.0f 5.0f 10.0f // Total shrink=20 allows ratio=-1.0f for one line
                Items.box 40.0f // "word3" - if alone on a line, it's underfull with no glue
            |]

        let options = LineBreakOptions.Default 70.0f
        let lines = LineBreaker.breakLines options items

        // Should choose to keep everything on one line (tight but feasible)
        // rather than creating an infeasible second line with no glue
        lines.Length |> shouldEqual 1

    [<Test>]
    let ``Hyphenation is preferred when trailing glue cannot stretch`` () =
        // If we break immediately after the glue (index 2), the line width is 30 on a 31-unit line
        // but there is zero glue remaining to stretch. computeAdjustmentRatio currently treats
        // that trailing glue as available, so the ratio looks perfect (diff=1, stretch=10 => 0.1)
        // even though TeX would see an underfull line. The hyphenated break (index 4) costs 100
        // but perfectly fills the line with actual boxes.
        let items =
            [|
                Items.box 20.0f
                Items.glue 10.0f 10.0f 3.0f
                Items.box 0.5f
                Items.penalty 0.5f 100.0f true
                Items.box 15.0f
                Items.glue 5.0f 5.0f 2.0f
                Items.box 10.0f
            |]

        let options = LineBreakOptions.Default 31.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 2
        lines.[0].Start |> shouldEqual 0
        // Under the bug, this is 2 (break after glue). With the fix it moves to 4.
        lines.[0].End |> shouldEqual 4

    [<Test>]
    let ``Trailing glue is excluded and cannot provide shrink`` () =
        // This test verifies that trailing glue is excluded from lines (TeX behavior).
        // Content is too wide to fit in one line even with all shrink (makes 2 lines optimal).
        //
        // - Break at position 2 (after glue):
        //   Line: box(55) + glue [TRAILING, excluded]
        //   Width = 55, shrink = 0, overfull for target 50 → REJECTED
        //
        // - Break at position 4 (after glue):
        //   Line 1: box(55) + glue(5,0,40) + box(15) + glue [TRAILING, excluded]
        //   Width = 75, shrink = 40 (glue[1] is NOT trailing now!)
        //   ratio = (50-75)/40 = -0.625, badness ≈ 24 < 200 → ACCEPTED
        //   Line 2: box(20) + glue(10,30,10) + box(5)
        //   Width = 35, stretch = 30, ratio = 0.5, badness = 12.5 → ACCEPTED
        //
        // - One-line alternative: width 110, shrink 50, ratio = -1.3, badness ≈ 220
        //   Much worse demerits than the two-line solution.
        //
        // The key: glue[1] provides shrink at position 4 but NOT at position 2 (trailing exclusion).

        let items =
            [|
                Items.box 55.0f
                Items.glue 5.0f 0.0f 40.0f // At pos 2: trailing (no shrink). At pos 4: NOT trailing (provides shrink)
                Items.box 15.0f
                Items.glue 5.0f 10.0f 0.0f // Trailing at position 4
                Items.box 20.0f
                Items.glue 10.0f 30.0f 10.0f // Stretch/shrink for the second line
                Items.box 5.0f
            |]

        let options = LineBreakOptions.Default 50.0f
        let lines = LineBreaker.breakLines options items

        // Should break at position 4 (position 2 is overfull due to trailing glue exclusion)
        lines.Length |> shouldEqual 2
        lines.[0].End |> shouldEqual 4

        // Verify Line 1 uses the non-trailing glue's shrink
        (abs (lines.[0].AdjustmentRatio - -0.625f)) < 0.02f |> shouldEqual true
