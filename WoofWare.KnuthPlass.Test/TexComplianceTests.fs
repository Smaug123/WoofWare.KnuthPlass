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
        let items =
            [|
                Items.box 30.0
                Items.glue 10.0 20.0 10.0 // Glue with natural width for line calculation
                Items.penalty 0.0 0.0 false // Explicit break point
                Items.box 30.0
                Items.glue 10.0 20.0 10.0
                Items.penalty 0.0 0.0 false // Explicit break point
                Items.box 30.0
            |]

        // With TeX's default line_penalty (10), the two-line layout has lower demerits.
        // Line 1: box(30) + glue(10) + penalty(0) = 40 width, stretch=20
        //   Target 50, ratio = (50-40)/20 = 0.5, badness ≈ 12.5
        //   Demerits = (10 + 12.5)^2 = 506
        // Line 2: box(30) + glue(10) + penalty(0) + box(30) = 70
        //   But wait, that's too long. Let me recalculate with better geometry.
        //
        // Actually, let's use a simpler approach: make both layouts clearly feasible
        // and show that line_penalty affects the choice.
        let lowPenalty = LineBreakOptions.Default 50.0
        let linesLow = LineBreaker.breakLines lowPenalty items
        linesLow.Length |> shouldEqual 2
        // The break should occur at the first penalty (position 3).
        linesLow.[0].End |> shouldEqual 3

        // With a very high line_penalty, TeX's formula pushes toward a single line.
        // Each additional line adds (line_penalty + badness)^2 to total demerits.
        // When line_penalty is huge, one tight line beats two normal lines.
        let highPenalty =
            { LineBreakOptions.Default 50.0 with
                LinePenalty = 10000.0
            }

        let linesHigh = LineBreaker.breakLines highPenalty items
        linesHigh.Length |> shouldEqual 1

    /// AdjacentLooseTightDemerits applies only when |fitness diff| > 1 (tex.web:16909).
    /// Here there are two feasible breaks:
    /// - Break at the penalty (diff=2): chosen when no adj_demerits are applied.
    /// - Break at the earlier glue (diff=0): chosen when adj_demerits are large.
    [<Test>]
    let ``Fitness diff over 1 is penalised, diff of 1 is not`` () =
        let items =
            [|
                Items.box 30.0
                Items.glue 10.0 20.0 0.0
                Items.box 10.0
                Items.penalty 0.0 0.0 false
                Items.glue 5.0 1.0 0.0
                Items.box 5.0
                Items.glue 5.0 1.0 0.0
                Items.box 5.0
            |]

        // Allow very loose lines so both candidates are feasible.
        let noPenalty =
            { LineBreakOptions.Default 60.0 with
                Tolerance = 20000.0
                AdjacentLooseTightDemerits = 0.0
            }

        let withPenalty =
            { LineBreakOptions.Default 60.0 with
                Tolerance = 20000.0
                AdjacentLooseTightDemerits = 1_000_000.0
            }

        let linesNoPenalty = LineBreaker.breakLines noPenalty items
        let linesWithPenalty = LineBreaker.breakLines withPenalty items

        // Without the penalty, the cheaper (but diff=2) break at the penalty is chosen.
        linesNoPenalty.Length |> shouldEqual 2
        linesNoPenalty.[0].End |> shouldEqual 4

        // With a huge adj_demerits, the algorithm should avoid the diff=2 transition
        // and instead break at the earlier glue (diff=1).
        linesWithPenalty.Length |> shouldEqual 2
        linesWithPenalty.[0].End |> shouldEqual 2

    /// When shrink is insufficient, TeX clamps glue_set to 1.0 (ratio = -1) instead of throwing.
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
