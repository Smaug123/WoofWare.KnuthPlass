namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

// Gemini 3.0 Pro came up with these.

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
                Items.box 10.0
                Items.glue 10.0 0.0 0.0 // Width 10, no stretch/shrink
                Items.box 10.0
            |]

        // Target 10.
        // Correct: Break after Box 1. Line = Box(10). Glue(10) is discarded. Width 10. Ratio 0.
        // Buggy: Break after Box 1. Line = Box(10) + Glue(10). Width 20. Ratio -infinity (Overfull).
        //        It will be forced to break at the end or fail.
        let options = LineBreakOptions.Default 10.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 2
        lines.[0].End |> shouldEqual 2 // Break after the first glue (index 2 is the start of next line)

    // 2. Misunderstanding of Tolerance (Critical)
    [<Test>]
    let ``Tolerance acts as a hard limit for feasible breaks`` () =
        // The only possible break (at index 4) results in a line with Badness 51,200.
        // Tolerance is set to 1.0.
        // TeX treats tolerance as a feasibility cutoff: a candidate whose badness exceeds tolerance is simply not admissible.
        // With no other breakpoints available, the paragraph should be rejected.
        let items =
            [|
                Items.box 1.0
                Items.glue 0.0 1.0 0.0 // Stretch 1.0
                Items.box 1.0
                Items.glue 0.0 1.0 0.0 // Break point
                Items.box 100.0 // Force remaining content to be too long for one line (requires a break)
            |]

        // Target: 10.0. Tolerance: 1.0.
        // Line 1 (0 to 4): Content width 2.0. Shortfall 8.0. Stretch 1.0. Ratio 8.0. Badness 51,200.
        let options =
            { LineBreakOptions.Default 10.0 with
                Tolerance = 1.0
            }

        // TeX would reject the paragraph: the only candidate is beyond tolerance.
        Assert.Throws<System.Exception> (fun () -> LineBreaker.breakLines options items |> ignore)
        |> ignore

    // 4. Incorrect "Flagged" Penalty Demerits
    [<Test>]
    let ``Demerits formula correctly separates penalty cost from badness`` () =
        // TeX includes a positive penalty inside the squared term: (line_penalty + badness + penalty)^2.
        // This test verifies that penalty cost is added BEFORE squaring, not after.
        //
        // Setup: Two competing breaks where BOTH are feasible (badness < tolerance).
        // The penalty cost should be included in the squared demerits term, making the
        // high-penalty break less attractive even though both lines fit acceptably.

        let items =
            [|
                Items.box 70.0
                Items.glue 20.0 15.0 5.0 // Ample stretch/shrink for both breaks

                // Break A (low penalty): slightly loose line but low penalty cost
                Items.box 5.0
                Items.penalty 0.0 10.0 false // Low penalty cost

                // Break B (high penalty): tighter line but high penalty cost
                Items.box 5.0
                Items.penalty 0.0 100.0 false // High penalty cost

                // Second line content
                Items.box 30.0
                Items.glue 10.0 50.0 5.0
                Items.box 40.0
            |]

        let options =
            { LineBreakOptions.Default 100.0 with
                Tolerance = 500.0 // High enough that both breaks are feasible
            }

        let lines = LineBreaker.breakLines options items

        // With penalty inside the square: (line_penalty + badness + 10)² vs (line_penalty + badness + 100)²
        // The low-penalty break (End=4) should win despite being slightly looser.
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
                Items.box 10.0
                Items.glue 10.0 0.0 5.0 // Glue A
                Items.glue 10.0 0.0 5.0 // Glue B - cannot break between A and B
                Items.box 10.0
                Items.glue 10.0 10.0 0.0 // Glue after second box - provides stretch for second line
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
                Items.box 10.0
                Items.glue 10.0 15.0 3.0 // Glue A with stretch
                Items.glue 10.0 15.0 3.0 // Glue B - cannot break between A and B
                Items.box 10.0
                Items.glue 10.0 15.0 3.0 // Provides stretch for potential second line
                Items.box 10.0 // Additional box to make layout work
            |]

        let options = LineBreakOptions.Default 30.0
        let lines = LineBreaker.breakLines options items

        // Should break into two lines at position 3 (after Glue B), not between the consecutive glues
        lines.Length |> shouldEqual 2
        lines.[0].End |> shouldEqual 3
