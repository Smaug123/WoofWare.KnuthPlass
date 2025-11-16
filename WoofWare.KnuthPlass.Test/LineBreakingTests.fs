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
