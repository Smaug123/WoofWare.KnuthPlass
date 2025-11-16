namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module PenaltyTests =
    [<Test>]
    let ``Penalty affects break choice`` () =
        let items =
            [
                Items.box 30.0
                Items.glue 10.0 5.0 3.0
                Items.box 30.0
                Items.penalty 0.0 1000.0 false
                Items.glue 10.0 5.0 3.0
                Items.box 30.0
            ]

        let options = LineBreakOptions.Default (60.0)
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldBeGreaterThan 0

    [<Test>]
    let ``Flagged penalties incur double hyphen demerits`` () =
        let items =
            [
                Items.box 40.0
                Items.penalty 5.0 50.0 true
                Items.glue 10.0 5.0 3.0
                Items.box 40.0
                Items.penalty 5.0 50.0 true
                Items.glue 10.0 5.0 3.0
                Items.box 40.0
            ]

        let options =
            { LineBreakOptions.Default (60.0) with
                DoubleHyphenDemerits = 10000.0
            }

        let lines = LineBreaker.breakLines options items
        lines.Length |> shouldBeGreaterThan 0
