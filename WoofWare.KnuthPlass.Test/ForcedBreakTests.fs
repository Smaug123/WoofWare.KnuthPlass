namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module ForcedBreakTests =
    [<Test>]
    let ``Forced break splits lines`` () =
        let items =
            [
                Items.box 30.0
                Items.glue 10.0 5.0 3.0
                Items.box 20.0
                Items.forcedBreak ()
                Items.box 40.0
            ]

        let options = LineBreakOptions.Default 100.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 2
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual 4
        lines.[1].Start |> shouldEqual 4
        lines.[1].End |> shouldEqual 5

    [<Test>]
    let ``Multiple forced breaks create multiple lines`` () =
        let items =
            [
                Items.box 20.0
                Items.forcedBreak ()
                Items.box 30.0
                Items.forcedBreak ()
                Items.box 40.0
            ]

        let options = LineBreakOptions.Default 100.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 3
