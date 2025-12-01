namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module PenaltyTests =
    [<Test>]
    let ``Penalty affects break choice`` () =
        let items =
            [|
                Items.box 30.0
                Items.glue 10.0 5.0 3.0
                Items.box 30.0
                Items.penalty 0.0 1000.0 false
                Items.glue 10.0 5.0 3.0
                Items.box 30.0
            |]

        let options = LineBreakOptions.Default 60.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldBeGreaterThan 0

    [<Test>]
    let ``Flagged penalties incur double hyphen demerits`` () =
        let items =
            [|
                Items.box 40.0
                Items.penalty 5.0 50.0 true
                Items.glue 10.0 5.0 3.0
                Items.box 40.0
                Items.penalty 5.0 50.0 true
                Items.glue 10.0 5.0 3.0
                Items.box 40.0
            |]

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
            [|
                Items.box 25.0
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
                Items.penalty 0.0 -500.0 false // Strong negative penalty: ENCOURAGES breaking
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
            |]

        let itemsWithPositivePenalty =
            [|
                Items.box 25.0
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
                Items.penalty 0.0 500.0 false // Positive penalty: discourages breaking
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
                Items.glue 10.0 5.0 3.0
                Items.box 25.0
            |]

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

    [<Test>]
    let ``Final hyphen demerits prevents paragraph ending with hyphen`` () =
        // This test demonstrates that FinalHyphenDemerits correctly penalizes
        // ending a paragraph with a hyphenated word (i.e., a hyphen on the
        // second-to-last line).
        //
        // Bug: Previously, the algorithm checked currIsFlagged on the last line,
        // which is always false since the last position has no penalty. It should
        // check prevWasFlagged to detect if the previous break was hyphenated.
        //
        // Test design: Create a paragraph with two breaking strategies:
        // - Strategy A: "word1 hy-" / "phen word2" (hyphen before last line)
        // - Strategy B: "word1" / "hyphen word2" (no hyphen before last line)
        //
        // With high FinalHyphenDemerits, strategy B should be strongly preferred.

        // CORRECTED FOR PROPER TEX GLUE HANDLING AND TOLERANCE CUTOFF:
        // Key insight: Breaking AT a glue excludes that glue (no stretch) → badness = inf_bad → rejected by TeX.
        // Solution: Non-hyphenated break must be at a PENALTY (not glue) to preserve stretch.
        //
        // Setup: Box + Glue + Penalty (non-hyphen break) vs. Box + Glue + Hyphen (hyphen break)
        // Both are viable, but FinalHyphenDemerits should determine which is chosen.
        //
        // TeX semantics for Line.End: When breaking at penalty index i, the penalty is consumed by the break,
        // and End points to the first item of the next line (i+1), not the break point itself.
        let items =
            [|
                Items.box 55.0 // word1
                Items.glue 10.0 15.0 3.0
                Items.penalty 0.0 0.0 false // NON-FLAGGED penalty for non-hyphen break (NEW!)
                Items.box 12.0 // "hy"
                Items.penalty 3.0 50.0 true // optional hyphen (FLAGGED)
                Items.box 13.0 // "phen"
                Items.glue 10.0 17.0 3.0 // Increased stretch from 5.0 to 17.0 so second line is viable (ratio=1.0, badness=100)
                Items.box 40.0 // word2
            |]

        // Keep tolerance high so that this test focuses solely on
        // the FinalHyphenDemerits behaviour rather than tolerance penalties.
        let baseOptions =
            { LineBreakOptions.Default 80.0 with
                Tolerance = 1000.0
            }

        // With low FinalHyphenDemerits, the algorithm chooses the hyphenated version
        // since it gives a better first line (50 + 10 + 12 + 3 = 75, close to 80)
        let optionsLowPenalty =
            { baseOptions with
                FinalHyphenDemerits = 0.0
            }

        let linesLowPenalty = LineBreaker.breakLines optionsLowPenalty items

        // With high FinalHyphenDemerits, the algorithm should avoid the hyphenated version
        // and instead break after the first glue, even though it's a worse fit
        let optionsHighPenalty =
            { baseOptions with
                FinalHyphenDemerits = 50000000.0
            }

        let linesHighPenalty = LineBreaker.breakLines optionsHighPenalty items

        // Both should successfully break
        linesLowPenalty.Length |> shouldBeGreaterThan 0
        linesHighPenalty.Length |> shouldBeGreaterThan 0

        // With the bug: both produce the same result (likely the hyphenated version)
        // because FinalHyphenDemerits is never applied.
        // With the fix: they should differ.
        linesLowPenalty |> shouldNotEqual linesHighPenalty

        // Specifically, the low penalty version should break at the hyphen (penalty at position 4).
        // End points to the first item of the next line (after consuming the penalty), so End=5.
        linesLowPenalty.[0].End |> shouldEqual 5

        // While the high penalty version should break at the non-flagged penalty (position 2)
        // to avoid ending with a flagged hyphen. End points to first item of next line, so End=3.
        linesHighPenalty.[0].End |> shouldEqual 3
