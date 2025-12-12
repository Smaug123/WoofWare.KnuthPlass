namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module BugReproductionTests =

    // Start of Gemini 3.0 Pro tests

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

    // 6. Invalid Breakpoint Logic (Consecutive Glues)
    [<Test>]
    let ``Cannot break between consecutive glues`` () =
        // TeX forbids breaking between consecutive glues (tex.web 16970-17115).
        // CORRECTED: Adjusted setup so the two-line solution is actually better than one-line.
        //
        // Previous setup: trailing glue exclusion made second line have no stretch (badness=inf_bad),
        // making two-line solution as bad as one-line solution with shrink ratio -2 (badness 800).
        //
        // New setup: Add glue AFTER the second-line box so it has stretch for fitting.
        let items =
            [|
                Items.box 10.0f
                Items.glue 10.0f 0.0f 5.0f // Glue A
                Items.glue 10.0f 0.0f 5.0f // Glue B - cannot break between A and B
                Items.box 10.0f
                Items.glue 10.0f 10.0f 0.0f // Glue after second box - provides stretch for second line
            |]

        // Target 20.
        // Breaking after Glue A would be perfect (10 + 10 = 20), but TeX forbids breaking between
        // consecutive glues. First feasible break is after Glue B (position 3).
        //
        // Analysis:
        // - Two lines (break at position 3):
        //   Line 1: box(10) + glue(10,0,5) + glue(10,0,5), trailing glue B excluded
        //           = box(10) + glue(10,0,5) = 20, perfect fit, badness=0, demerits=1
        //   Line 2: box(10) + glue(10,10,0), trailing glue excluded
        //           = box(10) = 10, target=20, need stretch=10 but no stretch (glue excluded!)
        //           badness=inf_bad, demerits=100,020,001
        //   Total: ~100,020,002
        //
        // - One line (break at position 5):
        //   box(10) + glue(10,0,5) + glue(10,0,5) + box(10) + glue(10,10,0), trailing glue excluded
        //   = box(10) + glue(10,0,5) + glue(10,0,5) + box(10) = 40
        //   target=20, need to shrink by 20, shrink=10, ratio=-2.0, badness=800
        //   demerits=(1 + 800)² = 641,601
        //
        // With these numbers, one-line is better (641,601 < 100,020,002)!
        // Need to make second line in two-line solution better by ensuring it has stretch.
        //
        // REVISED: Add another box and glue to create a viable second line
        let items =
            [|
                Items.box 10.0f
                Items.glue 10.0f 15.0f 3.0f // Glue A with stretch
                Items.glue 10.0f 15.0f 3.0f // Glue B - cannot break between A and B
                Items.box 10.0f
                Items.glue 10.0f 15.0f 3.0f // Provides stretch for potential second line
                Items.box 10.0f // Additional box to make layout work
            |]

        let options = LineBreakOptions.Default 30.0f
        let lines = LineBreaker.breakLines options items

        // Should break into two lines at position 3 (after Glue B), not between the consecutive glues
        lines.Length |> shouldEqual 2
        lines.[0].End |> shouldEqual 3

    // End of Gemini 3.0 Pro tests

    /// The line-breaker should prefer earlier valid breakpoints over later overfull ones.
    ///
    /// This test demonstrates a bug where the algorithm produces an overfull line when
    /// a valid breakpoint exists at an earlier position that would fit within the target width.
    ///
    /// Scenario:
    /// - Text: "Mr. Utterson the lawyer was a man of a rugged countenance"
    /// - Line width: 40
    /// - Valid breakpoint at position 22 (after "a"): width 38, fits perfectly
    /// - Algorithm incorrectly chooses position 26: width 45, overfull by 5
    ///
    /// The bug appears when additional text follows - with just "...rugged" (no "countenance"),
    /// the algorithm correctly breaks at position 22.
    [<Test>]
    let ``Should prefer valid breakpoint over later overfull one`` () =
        let text = "Mr. Utterson the lawyer was a man of a rugged countenance"
        let items = Items.fromEnglishString Text.defaultWordWidth Text.SPACE_WIDTH text
        let options = LineBreakOptions.Default 40.0f
        let lines = LineBreaker.breakLines options items

        // Calculate actual width of line 0
        let mutable boxWidth = 0.0f
        let mutable glueCount = 0

        for i = lines.[0].Start to lines.[0].End - 1 do
            match items.[i] with
            | Box b -> boxWidth <- boxWidth + b.Width
            | Glue _ -> glueCount <- glueCount + 1
            | Penalty _ -> ()

        // Exclude trailing glue
        if lines.[0].End > 0 && lines.[0].End <= items.Length then
            match items.[lines.[0].End - 1] with
            | Glue _ -> glueCount <- glueCount - 1
            | _ -> ()

        let actualWidth = boxWidth + float32 glueCount

        // The line should NOT be overfull. A valid breakpoint exists at position 22
        // with width 38, which fits in the target width of 40.
        //
        // Position 22 is after "a" in "of a", giving the line:
        // "Mr. Utterson the lawyer was a man of a" (38 chars, ratio ~0.5)
        //
        // The remaining text "rugged countenance" (18 chars) easily fits on line 2.
        actualWidth <= 40.0f |> shouldEqual true

        // Verify we break at the valid position, not the overfull one
        lines.[0].End |> shouldEqual 22
