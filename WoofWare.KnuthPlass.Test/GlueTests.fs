namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module GlueTests =
    [<Test>]
    let ``Glue stretches when line is too short`` () =
        let items = [| Items.box 30.0f ; Items.glue 10.0f 20.0f 5.0f ; Items.box 30.0f |]
        // Line width is 80, content is 70, so glue should stretch
        let options = LineBreakOptions.Default 80.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldBeGreaterThan 0.0f

    [<Test>]
    let ``Glue shrinks when line is too long`` () =
        let items = [| Items.box 40.0f ; Items.glue 30.0f 5.0f 10.0f ; Items.box 40.0f |]
        // Line width is 100, content is 110, so glue should shrink
        let options = LineBreakOptions.Default 100.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldBeSmallerThan 0.0f

    [<Test>]
    let ``Perfect fit has zero adjustment ratio`` () =
        let items = [| Items.box 40.0f ; Items.glue 20.0f 10.0f 5.0f ; Items.box 40.0f |]
        // Line width is exactly 100
        let options = LineBreakOptions.Default 100.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        (abs lines.[0].AdjustmentRatio < 1e-6f) |> shouldEqual true

    [<Test>]
    let ``Glue at start of line should be discarded from width calculation`` () =
        let items =
            [|
                Items.box 40.0f // "word" on first line
                Items.penalty 5.0f System.Single.NegativeInfinity false // forced break with hyphen width
                Items.glue 20.0f 10.0f 5.0f // space that should be discarded on second line
                Items.box 30.0f // "word" on second line
                Items.glue 10.0f 5.0f 3.0f // another glue
                Items.box 20.0f // another box
            |]

        let options = LineBreakOptions.Default 100.0f
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
        // the ratio is (100 - 60) / (5 + RightSkip.Stretch). Including the leading glue would
        // yield roughly 1.333 (with RightSkip.Stretch=0), which would keep the bug alive.
        let totalStretch = 5.0f + options.RightSkip.Stretch
        let expectedRatio = (options.LineWidth - 60.0f) / totalStretch
        secondLineRatio |> shouldEqual expectedRatio

    [<Test>]
    let ``RightSkip.Stretch is reflected in returned AdjustmentRatio for single-word lines`` () =
        // A single box has no inter-word glue. Without RightSkip.Stretch, the ratio
        // would be infinite. With RightSkip.Stretch = 4.0f, we expect:
        //   ratio = (lineWidth - boxWidth) / RightSkip.Stretch
        let boxWidth = 20.0f
        let lineWidth = 30.0f
        let rightSkipStretch = 4.0f

        let items = [| Items.box boxWidth |]

        let options =
            { LineBreakOptions.Default lineWidth with
                RightSkip =
                    {
                        Width = 0.0f
                        Stretch = rightSkipStretch
                        Shrink = 0.0f
                    }
            }

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1

        let expectedRatio = (lineWidth - boxWidth) / rightSkipStretch
        lines.[0].AdjustmentRatio |> shouldEqual expectedRatio

    [<Test>]
    let ``Glue break requires preceding box`` () =
        // TeX rule (tex.web:17109-17118): when cur_p is a glue_node, it's a legal breakpoint
        // iff auto_breaking and prev_p is not glue/penalty/explicit-kern/math.
        // In our simplified model: glue is a legal breakpoint iff preceded by a box.
        //
        // With consecutive glues [Box, Glue_a, Glue_b, Box]:
        // - Breaking after Glue_a (position 2) is LEGAL because Glue_a is preceded by Box
        // - Breaking after Glue_b (position 3) is ILLEGAL because Glue_b is preceded by Glue_a
        //
        // If the algorithm breaks at position 2, Glue_b ends up at the start of the next line
        // and is discarded (tex.web:17297, 17304).

        let items =
            [|
                Items.box 20.0f
                Items.glue 5.0f 1.0f 1.0f
                Items.glue 5.0f 1.0f 1.0f
                Items.box 20.0f
            |]

        let options = LineBreakOptions.Default 30.0f
        let lines = LineBreaker.breakLines options items

        // Verify all glue breaks follow the TeX rule: glue must be preceded by a box
        for line in lines do
            let breakIndex = line.End

            if breakIndex >= 2 && breakIndex < items.Length then
                match items.[breakIndex - 1] with
                | Item.Glue _ ->
                    // If we broke after a glue, verify the glue is preceded by a box
                    match items.[breakIndex - 2] with
                    | Item.Box _ -> () // Valid: glue preceded by box
                    | other -> failwithf "Glue at position %d not preceded by box, but by %A" (breakIndex - 1) other
                | _ -> ()
