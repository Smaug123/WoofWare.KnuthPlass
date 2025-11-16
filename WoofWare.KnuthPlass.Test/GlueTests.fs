namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module GlueTests =
    [<Test>]
    let ``Glue stretches when line is too short`` () =
        let items = [| Items.box 30.0 ; Items.glue 10.0 20.0 5.0 ; Items.box 30.0 |]
        // Line width is 80, content is 70, so glue should stretch
        let options = LineBreakOptions.Default 80.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldBeGreaterThan 0.0

    [<Test>]
    let ``Glue shrinks when line is too long`` () =
        let items = [| Items.box 40.0 ; Items.glue 30.0 5.0 10.0 ; Items.box 40.0 |]
        // Line width is 100, content is 110, so glue should shrink
        let options = LineBreakOptions.Default 100.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldBeSmallerThan 0.0

    [<Test>]
    let ``Perfect fit has zero adjustment ratio`` () =
        let items = [| Items.box 40.0 ; Items.glue 20.0 10.0 5.0 ; Items.box 40.0 |]
        // Line width is exactly 100
        let options = LineBreakOptions.Default 100.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        (abs lines.[0].AdjustmentRatio < 1e-6) |> shouldEqual true
