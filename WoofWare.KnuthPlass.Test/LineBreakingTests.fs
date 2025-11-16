namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module LineBreakingTests =
    [<Test>]
    let ``Long paragraph breaks into multiple lines`` () =
        let items =
            [
                Items.box 30.0
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
                Items.glue 10.0 5.0 3.0
                Items.box 30.0
                Items.glue 10.0 5.0 3.0
                Items.box 35.0
                Items.glue 10.0 5.0 3.0
                Items.box 20.0
            ]

        let options = LineBreakOptions.Default 80.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldBeGreaterThan 1

        // Check that lines don't overlap
        for i in 0 .. lines.Length - 2 do
            (lines.[i].End <= lines.[i + 1].Start) |> shouldEqual true

    [<Test>]
    let ``Breaking prefers balanced lines over greedy breaks`` () =
        // This tests the key feature of Knuth-Plass: it finds globally optimal breaks
        let items =
            [
                Items.box 20.0
                Items.glue 10.0 5.0 3.0
                Items.box 20.0
                Items.glue 10.0 5.0 3.0
                Items.box 20.0
                Items.glue 10.0 5.0 3.0
                Items.box 50.0
            ]

        let options = LineBreakOptions.Default 70.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 2

        // The algorithm should prefer more balanced lines
        for line in lines do
            (abs line.AdjustmentRatio < 2.0) |> shouldEqual true

    [<Test>]
    let ``Underfull line without glue is heavily penalized`` () =
        // This test demonstrates the bug where an underfull line with no glue
        // gets a perfect adjustment ratio (0.0) instead of being heavily penalized.
        //
        // Scenario: Three boxes that could break in two ways:
        // 1. Break after "word1 word2", leaving just "word3" alone on second line
        //    - Line 2 would be just Box(40) = 40 units on a 70-unit line with NO glue
        //    - This should be heavily penalized
        // 2. Keep "word1 word2 word3" together on one line
        //    - Total: 15 + 10 + 15 + 10 + 40 = 90 units on 70-unit line
        //    - Ratio = -20/9 ≈ -2.22, within default tolerance of 3.0
        //
        // With the bug, option 1 looks perfect (ratio=0.0, badness=0)
        // With the fix, option 1 is heavily penalized (ratio=1000.0, badness=1e9)
        // so the algorithm prefers option 2 despite it being tight
        let items =
            [
                Items.box 15.0 // "word1"
                Items.glue 10.0 5.0 3.0
                Items.box 15.0 // "word2"
                Items.glue 10.0 5.0 3.0
                Items.box 40.0 // "word3" - if alone on a line, it's underfull with no glue
            ]

        let options = LineBreakOptions.Default 70.0
        let lines = LineBreaker.breakLines options items

        // Should choose to keep everything on one line (tight but within tolerance)
        // rather than creating an underfull second line with no glue
        lines.Length |> shouldEqual 1
