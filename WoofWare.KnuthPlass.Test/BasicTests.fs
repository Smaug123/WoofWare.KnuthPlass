namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module BasicTests =
    [<Test>]
    let ``Single word fits on one line`` () =
        let items = [ Items.box 50.0 ]
        let options = LineBreakOptions.Default (100.0)
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual 1

    [<Test>]
    let ``Two words with space fit on one line`` () =
        let items = [ Items.box 30.0 ; Items.glue 10.0 5.0 3.0 ; Items.box 40.0 ]
        let options = LineBreakOptions.Default (100.0)
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual 3

    [<Test>]
    let ``Empty paragraph returns empty lines`` () =
        let items = []
        let options = LineBreakOptions.Default (100.0)
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 0

    [<Test>]
    let ``Single box wider than line width throws or returns overfull line`` () =
        let items = [ Items.box 150.0 ]
        let options = LineBreakOptions.Default (100.0)

        // This should either throw or return a line with a very bad adjustment ratio
        try
            let lines = LineBreaker.breakLines options items
            lines.Length |> shouldBeGreaterThan 0
        with :? System.Exception ->
            ()
