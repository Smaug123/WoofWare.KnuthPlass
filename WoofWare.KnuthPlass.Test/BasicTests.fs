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

    /// Regression test for floating-point precision issue in ratio >= -1.0 check.
    /// Cumulative sums can accumulate small errors (e.g., shrink=0.9999998808 instead of 1.0),
    /// causing ratio to be -1.0000001 instead of -1.0, which incorrectly fails the feasibility check.
    [<Test>]
    let ``Floating-point precision does not cause feasible break to be rejected`` () =
        // This test case was found by property-based testing.
        // The issue: line 14->22 has ratio exactly at the -1.0 boundary, but due to
        // accumulated floating-point errors in shrink (0.9999998808 instead of 1.0),
        // the ratio becomes -1.0000001192, which fails ratio >= -1.0f.
        let text = "zaonlji divdvp w yduxqrk hajq kqqgyr wtvbukx bvupitzy zbood lqgqfffk"
        let lineWidth = 23.0f

        let items = Items.fromEnglishString Text.defaultWordWidth Text.SPACE_WIDTH text
        let options = LineBreakOptions.Default lineWidth
        let lines = LineBreaker.breakLines options items

        // The algorithm should find the feasible 3-line solution (0->14->22->31),
        // not fall back to a single overfull line.
        lines.Length |> shouldBeGreaterThan 1

        // No line should be overfull (min width > line width)
        for line in lines do
            let mutable width = 0.0f
            let mutable shrink = 0.0f

            for i = line.Start to line.End - 1 do
                match items.[i] with
                | Box b -> width <- width + b.Width
                | Glue g ->
                    width <- width + g.Width
                    shrink <- shrink + g.Shrink
                | Penalty _ -> ()

            // Exclude trailing glue, add penalty width
            if line.End > 0 && line.End <= items.Length then
                match items.[line.End - 1] with
                | Glue g ->
                    width <- width - g.Width
                    shrink <- shrink - g.Shrink
                | Penalty p -> width <- width + p.Width
                | _ -> ()

            let minWidth = width - shrink
            // Allow small epsilon for floating-point comparison
            (minWidth <= lineWidth + 1e-5f)
            |> shouldEqual true
