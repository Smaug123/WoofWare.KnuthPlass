namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

// Gemini 3.0f Pro came up with these.

[<TestFixture>]
module BugReproductionTests =

    // 1. The "Break Point Glue" Calculation Error
    // This is covered by your existing test: ``Hyphenation is preferred when trailing glue cannot stretch``
    // However, here is a direct width verification test.
    [<Test>]
    let ``Trailing glue width is discarded from line calculation`` () =
        // We set up a line that is perfect ONLY if the trailing glue is discarded.
        // If trailing glue is included, the line is overfull.
        let items =
            [|
                Items.box 10.0f
                Items.glue 10.0f 0.0f 0.0f // Width 10, no stretch/shrink
                Items.box 10.0f
            |]

        // Target 10.
        // Correct: Break after Box 1. Line = Box(10). Glue(10) is discarded. Width 10. Ratio 0.
        // Buggy: Break after Box 1. Line = Box(10) + Glue(10). Width 20. Ratio -infinity (Overfull).
        //        It will be forced to break at the end or fail.
        let options = LineBreakOptions.Default 10.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 2
        lines.[0].End |> shouldEqual 2 // Break after the first glue (index 2 is the start of next line)

    // 2. Tolerance as a feasibility filter (but final pass rescues)
    [<Test>]
    let ``Tolerance filters mid-paragraph breaks but final pass rescues`` () =
        // TeX treats tolerance as a feasibility cutoff (tex.web:16320-16333): candidates whose
        // badness exceeds tolerance are pruned. However, on the final pass, TeX keeps a breakpoint
        // even when over threshold at the paragraph end (tex.web:16815-16829), producing an overfull
        // box rather than failing.
        //
        // This test creates a scenario where the mid-paragraph break exceeds tolerance,
        // so it gets pruned. TeX then falls back to a single overfull line.
        let items =
            [|
                Items.box 1.0f
                Items.glue 0.0f 1.0f 0.0f // Stretch 1.0
                Items.box 1.0f
                Items.glue 0.0f 1.0f 0.0f // Potential break point (badness 51200 at this position)
                Items.box 100.0f // Force remaining content to be too long for one line
            |]

        // Target: 10.0. Tolerance: 1.0.
        // Break at position 4: Content width 2.0. Shortfall 8.0. Stretch 2.0. Ratio 4.0. Badness 6400 > 1.
        // This break is pruned by tolerance.
        // TeX falls back to final-pass rescue: single overfull line.
        let options =
            { LineBreakOptions.Default 10.0f with
                Tolerance = 1.0f
            }

        // TeX does NOT reject - it produces an overfull line on the final pass.
        let lines = LineBreaker.breakLines options items
        lines.Length |> shouldEqual 1
        // The line is overfull (content > line width); we return -1.0f (our convention)
        lines.[0].AdjustmentRatio |> shouldEqual -1.0f

    // 4. Penalty contributes to demerits
    [<Test>]
    let ``Demerits formula adds penalty squared after base demerits`` () =
        // TeX's demerits formula (tex.web:16901-16905) for positive penalties is:
        //   demerits = (line_penalty + badness)² + penalty²
        // The penalty is added AFTER squaring the base, not inside the square.
        //
        // Setup: Two competing breaks where BOTH are feasible (badness < tolerance).
        // The higher penalty increases demerits via the + penalty² term, making the
        // high-penalty break less attractive.

        let items =
            [|
                Items.box 70.0f
                Items.glue 20.0f 15.0f 5.0f // Ample stretch/shrink for both breaks

                // Break A (low penalty): slightly loose line but low penalty cost
                Items.box 5.0f
                Items.penalty 0.0f 10.0f false // Low penalty cost

                // Break B (high penalty): tighter line but high penalty cost
                Items.box 5.0f
                Items.penalty 0.0f 100.0f false // High penalty cost

                // Second line content
                Items.box 30.0f
                Items.glue 10.0f 50.0f 5.0f
                Items.box 40.0f
            |]

        let options =
            { LineBreakOptions.Default 100.0f with
                Tolerance = 500.0f // High enough that both breaks are feasible
            }

        let lines = LineBreaker.breakLines options items

        // With penalty after square: (l+b)² + 10² vs (l+b)² + 100²
        // The low-penalty break (End=4) wins because 100² >> 10².
        lines.[0].End |> shouldEqual 4

    // 6. Glue breakpoint validity follows TeX rule
    [<Test>]
    let ``Glue breaks require preceding box per TeX rules`` () =
        // TeX rule (tex.web:17109-17118): glue is a legal breakpoint iff prev_p is not
        // glue/penalty/explicit-kern/math. In our model: glue must be preceded by a box.
        //
        // With [Box, Glue_a, Glue_b, Box, Glue, Box]:
        // - Position 2 (after Glue_a): VALID - Glue_a preceded by Box
        // - Position 3 (after Glue_b): INVALID - Glue_b preceded by Glue_a
        // - Position 5 (after Glue): VALID - Glue preceded by Box
        //
        // Consecutive glues at line start are discarded (tex.web:17297, 17304).

        let items =
            [|
                Items.box 10.0f
                Items.glue 10.0f 15.0f 3.0f // Glue A - can break after this (preceded by box)
                Items.glue 10.0f 15.0f 3.0f // Glue B - cannot break after this (preceded by Glue A)
                Items.box 10.0f
                Items.glue 10.0f 15.0f 3.0f // Can break after this (preceded by box)
                Items.box 10.0f
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

    // The algorithm should prefer a multi-line solution with low total demerits over
    // a single overfull line. When content exceeds line width but can be split into
    // lines that each fit well (badness within tolerance), the multi-line solution wins
    // because its total demerits are much lower than the inf_bad penalty for overfull.
    [<Test>]
    let ``Multi-line solution preferred over single overfull line`` () =
        // Three boxes with glue, total width 200 vs line width 150.
        // Single line: 200 vs 150 → overfull by 50, ratio < -1 → inf_bad (10000), demerits ~100M
        // Two lines (break at position 4):
        //   Line 1: 60+10+60 = 130, need 20 stretch, stretch=80 → ratio=0.25, badness≈2
        //   Line 2: 60, stretched to fill with infinite glue → ratio≈0, badness≈0
        //   Total demerits ≈ (10+2)² + (10+0)² ≈ 244
        // Clear winner: two lines (244) vs one overfull (100,200,100).
        let items =
            [|
                Items.box 60.0f
                Items.glue 10.0f 80.0f 5.0f
                Items.box 60.0f
                Items.glue 10.0f 80.0f 5.0f
                Items.box 60.0f
                Items.glue 0.0f infinityf 0.0f // finishing glue
                Items.forcedBreak ()
            |]

        let options = LineBreakOptions.Default 150.0f
        let lines = LineBreaker.breakLines options items

        // Should produce two lines, not one overfull line
        lines.Length |> shouldEqual 2
        // First line ends at position 4 (after second glue)
        lines.[0].End |> shouldEqual 4
        // Both lines should have reasonable adjustment ratios (not overfull)
        lines.[0].AdjustmentRatio |> shouldBeGreaterThan -1.0f
        lines.[1].AdjustmentRatio |> shouldBeGreaterThan -1.0f
