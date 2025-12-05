namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

/// Tests verifying compliance with TeX's Knuth-Plass semantics.
/// References are to tex.web line numbers.
[<TestFixture>]
module TexComplianceTests =

    [<Test>]
    let ``LineBreakOptions has LinePenalty with default 10`` () =
        let options = LineBreakOptions.Default 100.0
        options.LinePenalty |> shouldEqual 10.0

    /// High line_penalty should bias the algorithm toward fewer lines when both layouts are feasible.
    /// Note: Break points must be at penalties (not after glue) to preserve glue stretchability.
    /// When breaking after a glue, that glue is trailing and excluded from the line (tex.web:16517-16540).
    [<Test>]
    let ``High line penalty favours fewer lines`` () =
        // For this to be a valid TeX property test, BOTH layouts must be feasible:
        // - One-line layout: all content on one line with moderate stretching/shrinking
        // - Two-line layout: content split across two lines with moderate adjustments
        //
        // We need to ensure badness is within tolerance for both options.
        let items =
            [|
                Items.box 25.0
                Items.glue 10.0 15.0 10.0 // Provides stretch and shrink
                Items.penalty 0.0 0.0 false // Explicit break point (position 3)
                Items.box 25.0
                Items.glue 10.0 15.0 10.0
                Items.box 25.0
            |]

        // Line width 60.
        // One-line layout: 25 + 10 + 25 + 10 + 25 = 95 width, needs shrink = 35, shrink avail = 20
        //   Too overfull for one line at this width.
        //
        // Let's use line width 100 to make one-line feasible:
        // One-line: 25 + 10 + 25 + 10 + 25 = 95, stretch = 30, ratio = (100-95)/30 = 0.17, badness ≈ 0.5
        // Two-line:
        //   Line 1: 25 + 10 = 35, stretch = 15, ratio = (100-35)/15 = 4.33, badness ≈ 8100
        //   This is over default tolerance (200), so two-line won't be chosen unless tolerance is raised.

        // Use tolerance high enough for both layouts:
        let lowPenalty =
            { LineBreakOptions.Default 100.0 with
                Tolerance = 10000.0
            }

        let linesLow = LineBreaker.breakLines lowPenalty items

        // With default line_penalty (10) and both layouts feasible, two lines may be chosen
        // if total demerits are lower than one line's demerits.
        // Actually with the geometry above, one line is near-perfect (badness ~0.5), so
        // it should win even with default penalty.

        // Test the property: with VERY high line_penalty, the algorithm strongly prefers fewer lines.
        let highPenalty =
            { LineBreakOptions.Default 100.0 with
                LinePenalty = 50000.0
                Tolerance = 10000.0
            }

        let linesHigh = LineBreaker.breakLines highPenalty items

        // With high line_penalty, one line should be strongly preferred
        linesHigh.Length |> shouldEqual 1

        // The one-line layout should have a small positive ratio (stretching)
        (linesHigh.[0].AdjustmentRatio > 0.0 && linesHigh.[0].AdjustmentRatio < 1.0)
        |> shouldEqual true

    /// AdjacentLooseTightDemerits applies only when |fitness diff| > 1 (tex.web:16909).
    /// We create a scenario with two competing breaks:
    /// - Break A: Creates Loose→VeryLoose (from Start Normal → Loose diff=1, then Loose→VeryLoose diff=1) → NO penalty
    /// - Break B: Creates Normal→VeryLoose transition (from Start Normal → Normal diff=0, then Normal→VeryLoose diff=2) → penalty applies
    /// Without adj_demerits, Break B wins (lower base demerits).
    /// With huge adj_demerits, Break A wins (avoids the diff=2 penalty).
    [<Test>]
    let ``Fitness diff over 1 is penalised, diff of 1 is not`` () =
        // Parameters found by systematic search ensuring:
        // 1. 1-line solution is infeasible (total width exceeds line width with no shrink)
        // 2. Break A has fitness diff <= 1 for all transitions
        // 3. Break B has fitness diff > 1 for at least one transition
        // 4. Without adj_demerits: B wins
        // 5. With adj_demerits: A wins
        //
        // Line width 60.
        //
        // Break at position 3:
        //   Line 1: box(10) + glue(20) = 30 width, 30 stretch, ratio = (60-30)/30 = 1.0 → Loose (fit=2)
        //   Line 2: box(20) + glue(5) + box(10) = 35 width, 5 stretch, ratio = (60-35)/5 = 5.0 → VeryLoose (fit=3)
        //   Transitions: Start(Normal=1) → Loose(2) diff=1, Loose(2) → VeryLoose(3) diff=1 → NO adj_demerits
        //
        // Break at position 5:
        //   Line 1: box(10) + glue(20) + box(20) = 50 width, 30 stretch, ratio = (60-50)/30 = 0.33 → Normal (fit=1)
        //   Line 2: glue(5) + box(10) = 15 width, 5 stretch, ratio = (60-15)/5 = 9.0 → VeryLoose (fit=3)
        //   Transitions: Start(Normal=1) → Normal(1) diff=0, Normal(1) → VeryLoose(3) diff=2 → adj_demerits APPLIES

        let items =
            [|
                Items.box 10.0
                Items.glue 20.0 30.0 0.0
                Items.penalty 0.0 0.0 false // Break A: position 3
                Items.box 20.0
                Items.penalty 0.0 0.0 false // Break B: position 5
                Items.glue 5.0 5.0 0.0
                Items.box 10.0
            |]

        let noPenalty =
            { LineBreakOptions.Default 60.0 with
                Tolerance = 20000.0
                AdjacentLooseTightDemerits = 0.0
            }

        let withPenalty =
            { LineBreakOptions.Default 60.0 with
                Tolerance = 20000.0
                AdjacentLooseTightDemerits = 10_000_000.0
            }

        let linesNoPenalty = LineBreaker.breakLines noPenalty items
        let linesWithPenalty = LineBreaker.breakLines withPenalty items

        // Both should produce 2 lines
        linesNoPenalty.Length |> shouldEqual 2
        linesWithPenalty.Length |> shouldEqual 2

        // Without penalty, Break B (position 5) wins: lower base demerits
        linesNoPenalty.[0].End |> shouldEqual 5

        // With huge adj_demerits (10M), Break A (position 3) wins:
        // Avoids the Normal→VeryLoose (diff=2) penalty that Break B incurs
        linesWithPenalty.[0].End |> shouldEqual 3

    /// When shrink is insufficient, we clamp ratio to -1.0 (our convention for maximally compressed).
    [<Test>]
    let ``Overfull line with limited shrink clamps ratio to minus one`` () =
        let items = [| Items.box 80.0 ; Items.glue 10.0 0.0 5.0 ; Items.box 30.0 |]

        let options = LineBreakOptions.Default 100.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldEqual -1.0

    /// Leading glue after a breakpoint must be discarded when computing the displayed ratio (tex.web:17304-17319).
    [<Test>]
    let ``Adjustment ratio discards leading glue on the new line`` () =
        let items =
            [|
                Items.box 30.0
                Items.glue 10.0 10.0 0.0
                Items.box 10.0
                Items.penalty 0.0 0.0 false
                Items.glue 10.0 20.0 0.0 // Leading glue for the second line
                Items.box 10.0
                Items.glue 10.0 10.0 0.0
                Items.box 10.0
            |]

        // Line 1: 30 + 10 + 10 = 50, ratio = 1 (Loose)
        // Line 2 excluding leading glue: 10 + 10 + 10 = 30, stretch = 10, ratio = 3
        let options =
            { LineBreakOptions.Default 60.0 with
                Tolerance = 3000.0
            }

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 2
        lines.[0].End |> shouldEqual 4

        lines.[0].AdjustmentRatio |> shouldEqual 1.0
        lines.[1].AdjustmentRatio |> shouldEqual 3.0
