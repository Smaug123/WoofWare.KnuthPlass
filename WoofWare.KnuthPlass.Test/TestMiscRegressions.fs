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
        let options = { LineBreakOptions.Default 10.0 with Tolerance = 1.0 }

        // TeX would reject the paragraph: the only candidate is beyond tolerance.
        Assert.Throws<System.Exception> (fun () -> LineBreaker.breakLines options items |> ignore) |> ignore

    // 4. Incorrect "Flagged" Penalty Demerits
    [<Test>]
    let ``Demerits formula correctly separates penalty cost from badness`` () =
        // TeX includes a positive penalty inside the squared term: (line_penalty + badness + penalty)^2.
        // This scenario makes the optional penalty outweigh a slightly looser line, so the unpenalised break is preferred.

        let items =
            [|
                // Break B (no penalty): width 92.1, stretch 10 -> ratio 0.79 -> badness ≈ 49.3
                Items.box 80.0
                Items.glue 12.1 10.0 0.0

                // Break A (penalised): adding the next box reaches width 100 exactly, penalty 50.
                Items.box 8.0
                Items.penalty 0.0 50.0 false

                // Tail content for the second line
                Items.box 20.0
                Items.glue 72.0 72.0 0.0
            |]

        let options = LineBreakOptions.Default 100.0
        let lines = LineBreaker.breakLines options items

        // TeX prefers the unpenalised (looser) break at index 2; penalising inside the square tilts the choice.
        lines.[0].End |> shouldEqual 2

    // 6. Invalid Breakpoint Logic (Consecutive Glues)
    [<Test>]
    let ``Cannot break between consecutive glues`` () =
        // TeX does not permit a breakpoint immediately before another glue item.
        let items =
            [|
                Items.box 10.0
                Items.glue 10.0 0.0 5.0 // Glue A
                Items.glue 10.0 0.0 5.0 // Glue B
                Items.box 10.0
                Items.glue 10.0 10.0 0.0 // Trailing glue to let the second line stretch to the target
            |]

        // Target 20.
        // A break after the first glue would yield a perfect line (10 + 10), but TeX forbids
        // breaking between consecutive glues. The first feasible break is after Glue B.

        let options = LineBreakOptions.Default 20.0
        let lines = LineBreaker.breakLines options items

        // Two lines: the first includes both glues, the second starts at the final box.
        lines.Length |> shouldEqual 2
        lines.[0].End |> shouldEqual 3
