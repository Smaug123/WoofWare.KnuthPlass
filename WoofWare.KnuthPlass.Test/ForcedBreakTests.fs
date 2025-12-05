namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module ForcedBreakTests =
    [<Test>]
    let ``Forced break splits lines`` () =
        let items =
            [|
                Items.box 30.0
                Items.glue 10.0 5.0 3.0
                Items.box 20.0
                Items.forcedBreak ()
                Items.box 40.0
            |]

        let options = LineBreakOptions.Default 100.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 2
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual 4
        lines.[1].Start |> shouldEqual 4
        lines.[1].End |> shouldEqual 5

    [<Test>]
    let ``Trailing forced break bypasses tolerance`` () =
        // The last line requires significantly more shrink than is available.
        // A forced break must override tolerance checks and still allow the
        // paragraph to finish, otherwise we throw "No valid line breaking found".
        let items = [| Items.box 70.0 ; Items.glue 0.0 0.0 10.0 ; Items.forcedBreak () |]

        let options = LineBreakOptions.Default 50.0

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual items.Length

    [<Test>]
    let ``Forced break still ends paragraph when no shrink exists`` () =
        // Overfull with zero shrink: TeX emits an overfull hbox and clamps glue_set to 1.0
        // (tex.web:13104-13115). The ratio is clamped to -1.0, not -∞.
        let items = [| Items.box 70.0 ; Items.forcedBreak () |]

        let options = LineBreakOptions.Default 50.0

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual items.Length
        // Ratio is clamped to -1.0 for overfull lines, not -∞
        lines.[0].AdjustmentRatio |> shouldEqual -1.0

    [<Test>]
    let ``Forced break allows overfull line with limited shrink`` () =
        // Overfull, but some shrink exists. TeX clamps glue_set to 1.0, so ratio = -1.0.
        // The key point is that the break is ACCEPTED (explicit forced break allows overfull).
        let items = [| Items.box 80.0 ; Items.glue 0.0 0.0 5.0 ; Items.forcedBreak () |]

        let options = LineBreakOptions.Default 50.0

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual items.Length
        // Ratio is clamped to -1.0 for overfull lines (tex.web:13104-13115)
        lines.[0].AdjustmentRatio |> shouldEqual -1.0

    [<Test>]
    let ``Overfull without explicit forced break uses final-pass fallback`` () =
        // Same geometry as the forced-break test, but no explicit forced break.
        // TeX's final pass keeps an active node even for overfull lines at the paragraph end
        // (tex.web:16815-16829), producing an overfull hbox rather than failing.
        // The implicit paragraph-end forced break (eject_penalty) rescues the paragraph.
        let items = [| Items.box 80.0 ; Items.glue 0.0 0.0 5.0 |]

        let options = LineBreakOptions.Default 50.0

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual items.Length
        // Ratio is clamped to -1.0 for overfull lines (tex.web:13104-13115)
        lines.[0].AdjustmentRatio |> shouldEqual -1.0

    [<Test>]
    let ``Mid-paragraph forced break survives overfull first line`` () =
        // First line is overfull; forced break comes later. Ensure we still produce two lines.
        let items =
            [|
                Items.box 80.0
                Items.glue 0.0 0.0 5.0
                Items.box 10.0
                Items.forcedBreak ()
                Items.box 10.0
            |]

        let options = LineBreakOptions.Default 50.0

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 2
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual 4
        lines.[1].Start |> shouldEqual 4
        lines.[1].End |> shouldEqual 5

    [<Test>]
    let ``Multiple forced breaks create multiple lines`` () =
        let items =
            [|
                Items.box 20.0
                Items.forcedBreak ()
                Items.box 30.0
                Items.forcedBreak ()
                Items.box 40.0
            |]

        let options = LineBreakOptions.Default 100.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 3

    [<Test>]
    let ``FinalHyphenDemerits penalises hyphen on penultimate line`` () =
        // FinalHyphenDemerits adds extra cost when the second-to-last line ends at a flagged
        // penalty (tex.web:16911-16913). This test verifies that a sufficiently large
        // FinalHyphenDemerits value can change which break the algorithm chooses.
        //
        // Setup: Two competing break options where both produce 2 lines:
        // - Break at position 3 (flagged): line 1 = box(45) + glue(10) + penalty (flagged)
        // - Break at position 5 (unflagged): line 1 = box(45) + glue(10) + penalty + box(10) + penalty (unflagged)
        //
        // With low FinalHyphenDemerits, the tighter line (flagged break) wins.
        // With high FinalHyphenDemerits, the looser line (unflagged break) wins.
        let items =
            [|
                Items.box 45.0
                Items.glue 10.0 20.0 5.0
                Items.penalty 0.0 0.0 true // Position 3: flagged
                Items.box 10.0
                Items.penalty 0.0 0.0 false // Position 5: not flagged
                Items.glue 10.0 20.0 5.0
                Items.box 20.0
            |]

        let lowPenalty =
            { LineBreakOptions.Default 70.0 with
                FinalHyphenDemerits = 0.0
                Tolerance = 5000.0
            }

        let highPenalty =
            { LineBreakOptions.Default 70.0 with
                FinalHyphenDemerits = 10_000_000.0
                Tolerance = 5000.0
            }

        let linesLow = LineBreaker.breakLines lowPenalty items
        let linesHigh = LineBreaker.breakLines highPenalty items

        // Both should produce 2 lines
        linesLow.Length |> shouldEqual 2
        linesHigh.Length |> shouldEqual 2

        // With low penalty, the flagged break (position 3) may be chosen
        // With high penalty, the unflagged break (position 5) should be preferred
        linesLow.[0].End |> shouldEqual 3
        linesHigh.[0].End |> shouldEqual 5
