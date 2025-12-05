namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module BasicTests =
    [<Test>]
    let ``Single word fits on one line`` () =
        let items = [| Items.box 50.0f |]
        let options = LineBreakOptions.Default 100.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual 1

    [<Test>]
    let ``Two words with space fit on one line`` () =
        let items = [| Items.box 30.0f ; Items.glue 10.0f 5.0f 3.0f ; Items.box 40.0f |]
        let options = LineBreakOptions.Default 100.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual 3

    [<Test>]
    let ``Empty paragraph returns empty lines`` () =
        let items = [||]
        let options = LineBreakOptions.Default 100.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 0

    [<Test>]
    let ``Single box wider than line width produces overfull line`` () =
        // TeX does not fail on an overfull box. On the final pass it keeps an active node
        // and allows an overfull box rather than aborting (tex.web:16815-16829).
        // The paragraph succeeds with an overfull line.
        let items = [| Items.box 150.0f |]
        let options = LineBreakOptions.Default 100.0f

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual 1
        // Overfull with no shrink: we return -1.0f as our convention (maximally compressed)
        lines.[0].AdjustmentRatio |> shouldEqual -1.0f

    [<Test>]
    let foo () =
        let rng = System.Random (42) // Fixed seed for reproducibility

        let items = ResizeArray<Item> ()

        for _ in 1..50000 do
            // Add word with random width
            let wordWidth = 3.0f + rng.NextSingle () * 5.0f
            items.Add (Items.box wordWidth)

            // 10% chance of a hyphenation point
            if rng.NextDouble () < 0.1 then
                items.Add (Items.penalty 0.0f 50.0f true) // Flagged penalty for hyphen
                items.Add (Items.box (rng.NextSingle () * 5.0f))

            // Add space
            items.Add (Items.glue 1.0f 0.5f 0.2f)

        LineBreaker.breakLines (LineBreakOptions.Default 100.0f) (items.ToArray ())
        |> ignore

        ()
