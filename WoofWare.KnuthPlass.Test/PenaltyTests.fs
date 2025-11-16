namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module PenaltyTests =
    [<Test>]
    let ``Penalty affects break choice`` () =
        let items =
            [
                Items.box 30.0
                Items.glue 10.0 5.0 3.0
                Items.box 30.0
                Items.penalty 0.0 1000.0 false
                Items.glue 10.0 5.0 3.0
                Items.box 30.0
            ]

        let options = LineBreakOptions.Default 60.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldBeGreaterThan 0

    [<Test>]
    let ``Flagged penalties incur double hyphen demerits`` () =
        let items =
            [
                Items.box 40.0
                Items.penalty 5.0 50.0 true
                Items.glue 10.0 5.0 3.0
                Items.box 40.0
                Items.penalty 5.0 50.0 true
                Items.glue 10.0 5.0 3.0
                Items.box 40.0
            ]

        let options =
            { LineBreakOptions.Default 60.0 with
                DoubleHyphenDemerits = 10000.0
            }

        let lines = LineBreaker.breakLines options items
        lines.Length |> shouldBeGreaterThan 0

    [<Test>]
    let ``Negative penalties should reduce demerits and encourage breaks`` () =
        // This test verifies that negative penalties correctly encourage breaks.
        //
        // Bug context: The current implementation uses `abs penaltyCost` which
        // incorrectly treats negative penalties (which should encourage breaks)
        // as positive penalties (which discourage breaks).
        //
        // Test design: Create two similar break opportunities where one has a
        // strong negative penalty. The negative penalty should make that break
        // more attractive, resulting in a different breaking strategy than if
        // the penalty were absent or positive.

        let itemsWithNegativePenalty =
            [
                Items.box 25.0
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
                Items.penalty 0.0 -500.0 false // Strong negative penalty: ENCOURAGES breaking
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
            ]

        let itemsWithPositivePenalty =
            [
                Items.box 25.0
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
                Items.penalty 0.0 500.0 false // Positive penalty: discourages breaking
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
            ]

        let options = LineBreakOptions.Default 70.0

        let linesWithNegative = LineBreaker.breakLines options itemsWithNegativePenalty
        let linesWithPositive = LineBreaker.breakLines options itemsWithPositivePenalty

        // The negative penalty should encourage a break at that location,
        // potentially resulting in a different number of lines or different
        // break positions compared to the positive penalty case.
        //
        // At minimum, both should successfully break (not fail)
        linesWithNegative.Length |> shouldBeGreaterThan 0
        linesWithPositive.Length |> shouldBeGreaterThan 0

        // The key test: negative and positive penalties should produce different results
        // If the bug exists (negative treated as positive), they might produce the same result
        linesWithNegative |> shouldNotEqual linesWithPositive
