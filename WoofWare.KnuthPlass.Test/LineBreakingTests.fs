namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module LineBreakingTests =
    [<Test>]
    let ``Long paragraph breaks into multiple lines`` () =
        let items =
            [|
                Items.box 30.0
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
                Items.glue 10.0 5.0 3.0
                Items.box 30.0
                Items.glue 10.0 5.0 3.0
                Items.box 35.0
                Items.glue 10.0 5.0 3.0
                Items.box 20.0
            |]

        let options = LineBreakOptions.Default 80.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldBeGreaterThan 1

        // Check that lines don't overlap
        for i in 0 .. lines.Length - 2 do
            (lines.[i].End <= lines.[i + 1].Start) |> shouldEqual true

    [<Test>]
    let ``Breaking prefers balanced lines over greedy breaks`` () =
        // This tests the key feature of Knuth-Plass: it finds globally optimal breaks
        //
        // We have 6 boxes of width 8, separated by glue of width 2
        // With line width 30:
        // - Greedy break: fills first line to ~36 (needs shrinking), leaves second line at ~18 (needs huge stretching)
        // - Balanced break: splits evenly at 28 each (moderate stretching on both)
        //
        // Greedy: Line 1 width 36 → ratio (30-36)/4 = -1.5
        //         Line 2 width 18 → ratio (30-18)/1 = 12.0
        //         Total badness: 1.5² + 12² = 146.25
        //
        // Balanced: Line 1 width 28 → ratio (30-28)/2 = 1.0
        //           Line 2 width 28 → ratio (30-28)/2 = 1.0
        //           Total badness: 1² + 1² = 2.0

        let items =
            [|
                Items.box 8.0
                Items.glue 2.0 1.0 1.0 // width/stretch/shrink
                Items.box 8.0
                Items.glue 2.0 1.0 1.0
                Items.box 8.0
                Items.glue 2.0 1.0 1.0
                Items.box 8.0
                Items.glue 2.0 1.0 1.0
                Items.box 8.0
                Items.glue 2.0 1.0 1.0
                Items.box 8.0
            |]

        let options = LineBreakOptions.Default 30.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 2

        // The algorithm should prefer balanced lines (both around ratio 1.0)
        // over greedy breaking (ratios around -1.5 and 12.0)
        for line in lines do
            (abs line.AdjustmentRatio < 2.0) |> shouldEqual true

        // More specifically, both lines should have similar adjustment ratios
        let ratios = lines |> Array.map (fun l -> l.AdjustmentRatio)
        let maxRatio = ratios |> Array.max
        let minRatio = ratios |> Array.min
        (maxRatio - minRatio < 1.0) |> shouldEqual true // Lines should be similarly adjusted

    [<Test>]
    let ``Underfull line without glue is heavily penalized`` () =
        // This test demonstrates the bug where an underfull line with no glue
        // gets a perfect adjustment ratio (0.0) instead of being heavily penalized.
        //
        // Scenario: Three boxes that could break in two ways:
        // 1. Break after "word1 word2", leaving just "word3" alone on second line
        //    - Line 2 would be just Box(40) = 40 units on a 70-unit line with NO glue
        //    - This should be heavily penalized (ratio=1000.0, badness=1e11)
        // 2. Keep "word1 word2 word3" together on one line
        //    - Total: 15 + 10 + 15 + 10 + 40 = 90 units on 70-unit line
        //    - Need to shrink by 20, total shrink = 20
        //    - Ratio = -20/20 = -1.0, badness = 100
        //
        // With the bug, option 1 looks perfect (ratio=0.0, badness=0)
        // With the fix, option 1 is heavily penalized (ratio=1000.0, badness=1e11)
        // so the algorithm prefers option 2 despite it being tight
        let items =
            [|
                Items.box 15.0 // "word1"
                Items.glue 10.0 5.0 10.0 // Need shrink=10 to make one-line solution achievable
                Items.box 15.0 // "word2"
                Items.glue 10.0 5.0 10.0 // Total shrink=20 allows ratio=-1.0 for one line
                Items.box 40.0 // "word3" - if alone on a line, it's underfull with no glue
            |]

        let options = LineBreakOptions.Default 70.0
        let lines = LineBreaker.breakLines options items

        // Should choose to keep everything on one line (tight but within tolerance)
        // rather than creating an underfull second line with no glue
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
                Items.box 20.0
                Items.glue 10.0 10.0 3.0
                Items.box 0.5
                Items.penalty 0.5 100.0 true
                Items.box 15.0
                Items.glue 5.0 5.0 2.0
                Items.box 10.0
            |]

        let options = LineBreakOptions.Default 31.0
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
                Items.box 55.0
                Items.glue 5.0 0.0 40.0 // At pos 2: trailing (no shrink). At pos 4: NOT trailing (provides shrink)
                Items.box 15.0
                Items.glue 5.0 10.0 0.0 // Trailing at position 4
                Items.box 20.0
                Items.glue 10.0 30.0 10.0 // Stretch/shrink for the second line
                Items.box 5.0
            |]

        let options = LineBreakOptions.Default 50.0
        let lines = LineBreaker.breakLines options items

        // Should break at position 4 (position 2 is overfull due to trailing glue exclusion)
        lines.Length |> shouldEqual 2
        lines.[0].End |> shouldEqual 4

        // Verify Line 1 uses the non-trailing glue's shrink
        (abs (lines.[0].AdjustmentRatio - (-0.625))) < 0.02 |> shouldEqual true
