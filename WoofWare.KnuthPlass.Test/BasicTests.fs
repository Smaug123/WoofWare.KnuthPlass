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
    let ``Large paragraph with 50000 items completes without error`` () =
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

    /// Regression test: the algorithm should not produce an overfull line when a feasible solution exists.
    /// This case was found by property-based testing with text:
    /// "snwzmxz noume iuvwqqfwtl ppvrmgekwx aofpb xwtiycijr eiejo kmtwvjtgb ooantpfhnq bzlazhoxra rpjrglpvn fyyflb rxxeavtfcv toh nlzainy coygriefm"
    /// at line width 97.
    [<Test>]
    let ``No overfull line when feasible solution exists - regression`` () =
        // Explicit items from: Items.fromEnglishString Text.defaultWordWidth Text.SPACE_WIDTH
        // for text = "snwzmxz noume iuvwqqfwtl ppvrmgekwx aofpb xwtiycijr eiejo kmtwvjtgb ooantpfhnq bzlazhoxra rpjrglpvn fyyflb rxxeavtfcv toh nlzainy coygriefm"
        // Glue: width=1, stretch=0.5, shrink=1/3
        let items =
            [|
                Items.box 7.0f // snwzmxz
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 5.0f // noume
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 2.0f // iu (hyphen point)
                Items.penalty 1.0f 50.0f true
                Items.box 8.0f // vwqqfwtl
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 7.0f // ppvrmge (hyphen point)
                Items.penalty 1.0f 50.0f true
                Items.box 3.0f // kwx
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 2.0f // ao (hyphen point)
                Items.penalty 1.0f 50.0f true
                Items.box 3.0f // fpb
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 5.0f // xwtiy (hyphen point)
                Items.penalty 1.0f 50.0f true
                Items.box 4.0f // cijr
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 5.0f // eiejo
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 9.0f // kmtwvjtgb
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 3.0f // ooa (hyphen point)
                Items.penalty 1.0f 50.0f true
                Items.box 7.0f // ntpfhnq
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 4.0f // bzla (hyphen point)
                Items.penalty 1.0f 50.0f true
                Items.box 3.0f // zho (hyphen point)
                Items.penalty 1.0f 50.0f true
                Items.box 3.0f // xra
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 9.0f // rpjrglpvn
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 3.0f // fyy (hyphen point)
                Items.penalty 1.0f 50.0f true
                Items.box 3.0f // flb
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 5.0f // rxxea (hyphen point)
                Items.penalty 1.0f 50.0f true
                Items.box 5.0f // vtfcv
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 3.0f // toh
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 7.0f // nlzainy
                Items.glue 1.0f 0.5f (1.0f / 3.0f)
                Items.box 3.0f // coy (hyphen point)
                Items.penalty 1.0f 50.0f true
                Items.box 6.0f // griefm
                Items.glue 0.0f System.Single.PositiveInfinity 0.0f
                Items.forcedBreak ()
            |]

        let lineWidth = 97.0f
        let options = LineBreakOptions.Default lineWidth

        let lines = LineBreaker.breakLines options items

        // Helper: compute line width excluding trailing glue, including penalty width
        let computeLineWidth (startIdx : int) (endIdx : int) : float32 =
            let mutable width = 0.0f

            for i = startIdx to endIdx - 1 do
                match items.[i] with
                | Box b -> width <- width + b.Width
                | Glue g -> width <- width + g.Width
                | Penalty _ -> ()

            // Exclude trailing glue, but include penalty width (for hyphens)
            if endIdx > 0 && endIdx <= items.Length then
                match items.[endIdx - 1] with
                | Glue g -> width <- width - g.Width
                | Penalty p -> width <- width + p.Width
                | _ -> ()

            width

        // Helper: compute shrink available on a line, excluding trailing glue
        let computeLineShrink (startIdx : int) (endIdx : int) : float32 =
            let mutable shrink = 0.0f

            for i = startIdx to endIdx - 1 do
                match items.[i] with
                | Glue g -> shrink <- shrink + g.Shrink
                | _ -> ()

            // Exclude trailing glue
            if endIdx > 0 && endIdx <= items.Length then
                match items.[endIdx - 1] with
                | Glue g -> shrink <- shrink - g.Shrink
                | _ -> ()

            shrink

        // Check if any line is overfull (width exceeds target even with maximum shrink)
        let isOverfull (startIdx : int) (endIdx : int) : bool =
            let width = computeLineWidth startIdx endIdx
            let shrink = computeLineShrink startIdx endIdx
            let minPossibleWidth = width - shrink
            minPossibleWidth > lineWidth + 1e-6f

        // Assert no line is overfull - a feasible solution exists for this input
        for line in lines do
            if isOverfull line.Start line.End then
                failwithf
                    "Overfull line from %d to %d: width=%.2f, shrink=%.2f, min=%.2f > lineWidth=%.2f"
                    line.Start
                    line.End
                    (computeLineWidth line.Start line.End)
                    (computeLineShrink line.Start line.End)
                    (computeLineWidth line.Start line.End - computeLineShrink line.Start line.End)
                    lineWidth

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

        // The algorithm should find a multi-line solution (not a single overfull line).
        // The exact number of lines may vary based on optimization, but all must fit.
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
            (minWidth <= lineWidth + 1e-5f) |> shouldEqual true
