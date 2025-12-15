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
                Items.box 30.0f
                Items.glue 10.0f 5.0f 3.0f
                Items.box 20.0f
                Items.forcedBreak ()
                Items.box 40.0f
            |]

        let options = LineBreakOptions.Default 100.0f
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
        let items =
            [| Items.box 70.0f ; Items.glue 0.0f 0.0f 10.0f ; Items.forcedBreak () |]

        let options = LineBreakOptions.Default 50.0f

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual items.Length

    [<Test>]
    let ``Forced break still ends paragraph when no shrink exists`` () =
        // Overfull with zero shrink: we return -1.0f as our convention for "maximally compressed".
        // Note: TeX's hpack sets glue_sign to "normal" when there's no shrink, so it doesn't
        // actually produce a -1.0f ratio internally, but we need a usable value for our API.
        let items = [| Items.box 70.0f ; Items.forcedBreak () |]

        let options = LineBreakOptions.Default 50.0f

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual items.Length
        // Our convention: -1.0f for overfull lines (maximally compressed)
        lines.[0].AdjustmentRatio |> shouldEqual -1.0f

    [<Test>]
    let ``Forced break allows overfull line with limited shrink`` () =
        // Overfull, but some shrink exists. Ratio is clamped to -1.0f (maximally compressed).
        // The key point is that the break is ACCEPTED (explicit forced break allows overfull).
        let items = [| Items.box 80.0f ; Items.glue 0.0f 0.0f 5.0f ; Items.forcedBreak () |]

        let options = LineBreakOptions.Default 50.0f

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual items.Length
        // Ratio clamped to -1.0f for overfull lines (our convention for maximally compressed)
        lines.[0].AdjustmentRatio |> shouldEqual -1.0f

    [<Test>]
    let ``Overfull without explicit forced break uses final-pass fallback`` () =
        // Same geometry as the forced-break test, but no explicit forced break.
        // TeX's final pass keeps an active node even for overfull lines at the paragraph end
        // (tex.web:16815-16829), producing an overfull hbox rather than failing.
        // The implicit paragraph-end forced break (eject_penalty) rescues the paragraph.
        let items = [| Items.box 80.0f ; Items.glue 0.0f 0.0f 5.0f |]

        let options = LineBreakOptions.Default 50.0f

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 1
        lines.[0].Start |> shouldEqual 0
        lines.[0].End |> shouldEqual items.Length
        // Ratio clamped to -1.0f for overfull lines (our convention for maximally compressed)
        lines.[0].AdjustmentRatio |> shouldEqual -1.0f

    [<Test>]
    let ``Mid-paragraph forced break survives overfull first line`` () =
        // First line is overfull; forced break comes later. Ensure we still produce two lines.
        let items =
            [|
                Items.box 80.0f
                Items.glue 0.0f 0.0f 5.0f
                Items.box 10.0f
                Items.forcedBreak ()
                Items.box 10.0f
            |]

        let options = LineBreakOptions.Default 50.0f

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
                Items.box 20.0f
                Items.forcedBreak ()
                Items.box 30.0f
                Items.forcedBreak ()
                Items.box 40.0f
            |]

        let options = LineBreakOptions.Default 100.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 3

    [<Test>]
    let ``FinalHyphenDemerits penalises hyphen on penultimate line`` () =
        // FinalHyphenDemerits adds extra cost when the second-to-last line ends at a flagged
        // penalty (tex.web:16906-16908). This test verifies that a sufficiently large
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
                Items.box 45.0f
                Items.glue 10.0f 20.0f 5.0f
                Items.penalty 0.0f 0.0f true // Position 3: flagged
                Items.box 10.0f
                Items.penalty 0.0f 0.0f false // Position 5: not flagged
                Items.glue 10.0f 20.0f 5.0f
                Items.box 20.0f
            |]

        let lowPenalty =
            { LineBreakOptions.Default 70.0f with
                FinalHyphenDemerits = 0.0f
                Tolerance = 5000.0f
            }

        let highPenalty =
            { LineBreakOptions.Default 70.0f with
                FinalHyphenDemerits = 10_000_000.0f
                Tolerance = 5000.0f
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

    /// Ensure the algorithm produces output when multiple active nodes all become overfull
    /// and are deactivated. The rescue mechanism must handle this case.
    [<Test>]
    let ``Algorithm survives multiple overfull nodes being deactivated`` () =
        let items =
            [|
                Items.box 20.0f
                Items.glue 5.0f 10.0f 2.0f
                Items.box 20.0f
                Items.glue 5.0f 10.0f 2.0f
                Items.box 300.0f // Massive box that makes all paths overfull
            |]

        let options = LineBreakOptions.Default 60.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldBeGreaterThan 0
        let last = Array.last lines
        last.End |> shouldEqual items.Length

    /// Ensure the algorithm survives when multiple active nodes all become overfull
    /// at the paragraph-end forced break (all paths exceed tolerance).
    [<Test>]
    let ``Algorithm survives when all active nodes become overfull at paragraph end`` () =
        // Multiple breakpoints exist in the middle, but the massive final box
        // causes all active paths to become overfull when we reach the paragraph end.
        // The implicit paragraph-end forced break must rescue the algorithm.
        let items =
            [|
                Items.box 10.0f
                Items.glue 5.0f 5.0f 2.0f
                Items.penalty 0.0f 0.0f false
                Items.box 10.0f
                Items.glue 5.0f 5.0f 2.0f
                Items.penalty 0.0f 0.0f false
                Items.box 10.0f
                Items.glue 5.0f 5.0f 2.0f
                Items.box 500.0f // Massive box - all paths become overfull
            |]

        let options =
            { LineBreakOptions.Default 50.0f with
                Tolerance = 500.0f
            }

        let lines = LineBreaker.breakLines options items
        lines.Length |> shouldBeGreaterThan 0
        let last = Array.last lines
        last.End |> shouldEqual items.Length

    /// Verify implicit paragraph-end forced break produces output when competing paths exist.
    /// Note: This test does NOT use an explicit Items.forcedBreak(); it relies on the
    /// implicit paragraph-end forced break (eject_penalty) that TeX always appends.
    [<Test>]
    let ``Implicit paragraph-end forced break handles competing paths`` () =
        // Two penalties create competing break paths with different costs.
        // The massive final box makes all paths overfull, requiring the implicit
        // paragraph-end forced break to rescue the algorithm.
        let items =
            [|
                Items.box 40.0f
                Items.glue 10.0f 5.0f 5.0f
                Items.penalty 0.0f 0.0f false
                Items.box 40.0f
                Items.glue 10.0f 5.0f 5.0f
                Items.penalty 0.0f 100.0f false
                Items.box 200.0f // Massive final box
            |]

        let options = LineBreakOptions.Default 100.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldBeGreaterThan 0
        let last = Array.last lines
        last.End |> shouldEqual items.Length
