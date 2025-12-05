namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module BasicTests =
    [<Test>]
    let ``Single word fits on one line`` () =
        let items = [| Items.box 50.0 |]
        let options = LineBreakOptions.Default 100.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual 1

    [<Test>]
    let ``Two words with space fit on one line`` () =
        let items = [| Items.box 30.0 ; Items.glue 10.0 5.0 3.0 ; Items.box 40.0 |]
        let options = LineBreakOptions.Default 100.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual 3

    [<Test>]
    let ``Empty paragraph returns empty lines`` () =
        let items = [||]
        let options = LineBreakOptions.Default 100.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 0

    [<Test>]
    let ``Single box wider than line width produces overfull line`` () =
        // TeX does not fail on an overfull box. On the final pass it keeps an active node
        // and allows an overfull box rather than aborting (tex.web:16815-16829).
        // The paragraph succeeds with an overfull line.
        let items = [| Items.box 150.0 |]
        let options = LineBreakOptions.Default 100.0

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual 1
        // Overfull with no shrink: we return -1.0 as our convention (maximally compressed)
        lines.[0].AdjustmentRatio |> shouldEqual -1.0
