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
        let options = LineBreakOptions.Default 100.0f
        options.LinePenalty |> shouldEqual 10.0f

    /// High line_penalty should bias the algorithm toward fewer lines when both layouts are feasible.
    /// Note: Break points must be at penalties (not after glue) to preserve glue stretchability.
    /// When breaking after a glue, that glue is trailing and excluded from the line (tex.web:16517-16540).
    [<Test>]
    let ``High line penalty favours fewer lines`` () =
        // For this to be a valid test, BOTH layouts must be feasible and their demerits
        // must be comparable enough that LinePenalty can swing the decision.
        //
        // Geometry: Line width = 100.
        //
        // Items: Box(35) - Glue(10, stretch=100, shrink=50) - Box(35) - Penalty - Box(35) - Glue(10, stretch=100, shrink=50) - Box(35)
        //
        // One-line: natural width = 35+10+35+35+10+35 = 160, shrink = 100
        //   ratio = (100-160)/100 = -0.6, badness = 100 * 0.6³ ≈ 21.6
        //
        // Two-line (break at position 4, the penalty):
        //   Line 1: 35+10+35 = 80, stretch = 100, ratio = (100-80)/100 = 0.2, badness ≈ 0.8
        //   Line 2: 35+10+35 = 80, stretch = 100, ratio = (100-80)/100 = 0.2, badness ≈ 0.8
        //
        // Demerits comparison:
        //   One-line:  (LP + 21.6)²
        //   Two-line:  2 * (LP + 0.8)²
        //
        // Crossover when (LP + 21.6)² = 2*(LP + 0.8)²
        // Solving: LP ≈ 49.4
        //
        // So: LP < 50 → two-line wins; LP > 50 → one-line wins.
        let items =
            [|
                Items.box 35.0f
                Items.glue 10.0f 100.0f 50.0f // High stretch and shrink
                Items.box 35.0f
                Items.penalty 0.0f 0.0f false // Break point at position 4
                Items.box 35.0f
                Items.glue 10.0f 100.0f 50.0f
                Items.box 35.0f
            |]

        let lowPenalty =
            { LineBreakOptions.Default 100.0f with
                LinePenalty = 10.0f // Well below crossover (~49)
                Tolerance = 1000.0f
            }

        let highPenalty =
            { LineBreakOptions.Default 100.0f with
                LinePenalty = 100.0f // Well above crossover (~49)
                Tolerance = 1000.0f
            }

        let linesLow = LineBreaker.breakLines lowPenalty items
        let linesHigh = LineBreaker.breakLines highPenalty items

        // With low LinePenalty, two-line solution wins (lower total badness outweighs extra line cost)
        linesLow.Length |> shouldEqual 2

        // With high LinePenalty, one-line solution wins (extra line penalty dominates)
        linesHigh.Length |> shouldEqual 1

        // Verify the one-line solution has the expected negative ratio (shrinking)
        (linesHigh.[0].AdjustmentRatio < 0.0f && linesHigh.[0].AdjustmentRatio > -1.0f)
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
        //   Line 1: box(10) + glue(20) = 30 width, 30 stretch, ratio = (60-30)/30 = 1.0f → Loose (fit=2)
        //   Line 2: box(20) + glue(5) + box(10) = 35 width, 5 stretch, ratio = (60-35)/5 = 5.0f → VeryLoose (fit=3)
        //   Transitions: Start(Normal=1) → Loose(2) diff=1, Loose(2) → VeryLoose(3) diff=1 → NO adj_demerits
        //
        // Break at position 5:
        //   Line 1: box(10) + glue(20) + box(20) = 50 width, 30 stretch, ratio = (60-50)/30 = 0.33 → Normal (fit=1)
        //   Line 2: glue(5) + box(10) = 15 width, 5 stretch, ratio = (60-15)/5 = 9.0f → VeryLoose (fit=3)
        //   Transitions: Start(Normal=1) → Normal(1) diff=0, Normal(1) → VeryLoose(3) diff=2 → adj_demerits APPLIES

        let items =
            [|
                Items.box 10.0f
                Items.glue 20.0f 30.0f 0.0f
                Items.penalty 0.0f 0.0f false // Break A: position 3
                Items.box 20.0f
                Items.penalty 0.0f 0.0f false // Break B: position 5
                Items.glue 5.0f 5.0f 0.0f
                Items.box 10.0f
            |]

        let noPenalty =
            { LineBreakOptions.Default 60.0f with
                Tolerance = 20000.0f
                AdjacentLooseTightDemerits = 0.0f
            }

        let withPenalty =
            { LineBreakOptions.Default 60.0f with
                Tolerance = 20000.0f
                AdjacentLooseTightDemerits = 10_000_000.0f
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

    /// When shrink is insufficient, we clamp ratio to -1.0f (our convention for maximally compressed).
    /// Uses a single box wider than the line to ensure overfull is unavoidable.
    [<Test>]
    let ``Overfull line with limited shrink clamps ratio to minus one`` () =
        // Single box wider than line width - no feasible alternative exists
        let items = [| Items.box 150.0f |]

        let options = LineBreakOptions.Default 100.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldEqual -1.0f

    /// Leading glue after a breakpoint must be discarded when computing the displayed ratio (tex.web:17304-17319).
    [<Test>]
    let ``Adjustment ratio discards leading glue on the new line`` () =
        let items =
            [|
                Items.box 30.0f
                Items.glue 10.0f 10.0f 0.0f
                Items.box 10.0f
                Items.penalty 0.0f 0.0f false
                Items.glue 10.0f 20.0f 0.0f // Leading glue for the second line
                Items.box 10.0f
                Items.glue 10.0f 10.0f 0.0f
                Items.box 10.0f
            |]

        // Line 1: 30 + 10 + 10 = 50, ratio = 1 (Loose)
        // Line 2 excluding leading glue: 10 + 10 + 10 = 30, stretch = 10, ratio = 3
        let options =
            { LineBreakOptions.Default 60.0f with
                Tolerance = 3000.0f
            }

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 2
        lines.[0].End |> shouldEqual 4

        lines.[0].AdjustmentRatio |> shouldEqual 1.0f
        lines.[1].AdjustmentRatio |> shouldEqual 3.0f
