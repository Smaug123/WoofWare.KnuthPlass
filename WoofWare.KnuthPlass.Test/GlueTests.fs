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

    [<Test>]
    let ``Glue at start of line should be discarded from width calculation`` () =
        let items =
            [|
                Items.box 40.0 // "word" on first line
                Items.penalty 5.0 (-infinity) false // forced break with hyphen width
                Items.glue 20.0 10.0 5.0 // space that should be discarded on second line
                Items.box 30.0 // "word" on second line
                Items.glue 10.0 5.0 3.0 // another glue
                Items.box 20.0 // another box
            |]

        let options = LineBreakOptions.Default 100.0
        let lines = LineBreaker.breakLines options items

        // Should break into 2 lines at the forced break
        lines.Length |> shouldEqual 2

        // First line: Box(40) + Penalty(5) = 45
        // adjustment ratio = (100 - 45) / 0 = infinity (but we expect it to be handled)
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual 2

        // Second line should start immediately after the forced break.
        lines.[1].Start |> shouldEqual 2

        let secondLineRatio = lines.[1].AdjustmentRatio

        // Glue at position 2 is discardable, so the second line only contains:
        //   Box(30) + Glue(10 stretch 5 shrink 3) + Box(20)
        // The resulting width is 60 with 5 units of stretch. With a 100-unit target width,
        // the ratio must therefore be (100 - 60) / 5 = 8.0. Including the leading glue would
        // yield roughly 1.333, which keeps the bug alive.
        let expectedRatio = (options.LineWidth - 60.0) / 5.0
        secondLineRatio |> shouldEqual expectedRatio

    [<Test>]
    let ``Cannot break between two consecutive glues`` () =
        // Scenario:
        // Box(10) + Glue(10) + Glue(10) + Box(10)
        // Target Width = 30.
        //
        // Option A (Legal): Break at first Glue.
        // Line 1: Box(10). Discard Glue(10).
        // Line 2 starts with Glue(10) -> Discarded by "Start of line" fix.
        // Line 2 content: Box(10).
        // This is very loose (Width 10 vs Target 30).
        //
        // Option B (Illegal): Break at second Glue.
        // Line 1: Box(10) + Glue(10). Width = 20. Target = 30.
        // Line 2: Box(10). Width = 10. Target = 30.
        //
        // WAIT. This logic depends on how "Start of line" logic is implemented.
        // Let's look at it strictly as "Previous item was glue".
        //
        // Let's use a clearer restriction:
        // Box(20) Glue(5) Glue(5) Box(20). Target = 30.
        //
        // If we break at Glue 2 (Index 2):
        // Line 1 contains Box(20) + Glue(5). Width 25.
        // Target 30. This is a decent fit.
        //
        // If we break at Glue 1 (Index 1):
        // Line 1 contains Box(20). Width 20.
        // Target 30. This is a worse fit.
        //
        // The algorithm will prefer breaking at Index 2 (Glue 2) because 25 is closer to 30 than 20 is.
        // But breaking at Index 2 should be impossible because Index 1 is Glue.

        let items =
            [|
                Items.box 20.0
                Items.glue 5.0 1.0 1.0
                Items.glue 5.0 1.0 1.0
                Items.box 20.0
            |]

        let options = LineBreakOptions.Default 30.0
        let lines = LineBreaker.breakLines options items

        // If the bug exists, it will break at index 2 (after the first glue, eating the second),
        // or index 3 (after second glue).
        // We want to ensure it does NOT break at index 2 if index 1 was glue.

        // Actually, let's simplify the assertion:
        // Iterate through all lines. If a line ends at `i`, `items[i-1]` is the break point.
        // If `items[i-1]` is Glue, then `items[i-2]` must NOT be Glue.

        for line in lines do
            let breakIndex = line.End

            if breakIndex > 0 && breakIndex < items.Length then
                match items.[breakIndex - 1], items.[breakIndex] with
                | Item.Glue _, Item.Glue _ -> failwith "Found a break between two glues!"
                | _ -> ()
