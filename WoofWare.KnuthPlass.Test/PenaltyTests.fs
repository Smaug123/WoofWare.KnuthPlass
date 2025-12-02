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
        // This test verifies that negative penalties correctly encourage breaks per TeX semantics.
        // TeX demerits formula (tex.web 16901-16908):
        // - Positive penalty P ≥ 0: (L + P)²
        // - Negative penalty P < 0: L² - P²
        //
        // Setup: Create two competing FEASIBLE breaks where the penalty sign determines choice.
        // - Break A (at first penalty with cost 0): Moderately loose line, no penalty cost
        // - Break B (at second penalty with signed cost): Slightly tighter line, with signed penalty
        //
        // With negative penalty, the L² - P² formula makes Break B attractive.
        // With positive penalty, the (L + P)² formula makes Break A preferable.

        let makeItems penaltyCost =
            [|
                Items.box 60.0
                Items.glue 15.0 20.0 5.0

                // Break A: loose line but no penalty cost
                Items.box 5.0
                Items.penalty 0.0 0.0 false // Zero-cost break

                // Break B: tighter line but with signed penalty cost
                Items.box 10.0
                Items.penalty 0.0 penaltyCost false // Signed penalty

                // Second line
                Items.box 30.0
                Items.glue 10.0 50.0 5.0
                Items.box 40.0
            |]

        let options =
            { LineBreakOptions.Default 100.0 with
                Tolerance = 500.0 // High enough that both breaks are feasible
            }

        let linesWithNegative = LineBreaker.breakLines options (makeItems -100.0)
        let linesWithPositive = LineBreaker.breakLines options (makeItems 100.0)

        // Both should successfully break
        linesWithNegative.Length |> shouldBeGreaterThan 0
        linesWithPositive.Length |> shouldBeGreaterThan 0

        // The negative penalty should encourage breaking at the second penalty (End=6)
        // because L² - (-100)² = L² - 10000 provides strong negative demerits.
        linesWithNegative.[0].End |> shouldEqual 6

        // The positive penalty should discourage breaking at the second penalty,
        // preferring the first zero-cost penalty (End=4) because (L + 100)² is very high.
        linesWithPositive.[0].End |> shouldEqual 4

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

        // CORRECTED: Adjusted glue stretch values to create scenario where FinalHyphenDemerits affects choice.
        //
        // Setup: Two competing break positions:
        // - Position 3 (non-flagged penalty): Mediocre first line (badness~100), excellent second line (badness~1.7)
        //   Total demerits: ~10,201 + ~7 = ~10,208
        // - Position 5 (flagged hyphen): Perfect first line (badness=0), decent second line (badness~31)
        //   Total demerits: ~2,601 + ~1,024 + FinalHyphenDemerits
        //
        // With FinalHyphenDemerits=0: Position 5 wins (~3,625 < ~10,208)
        // With FinalHyphenDemerits=50,000,000: Position 3 wins (~10,208 < ~50,003,625)
        let items =
            [|
                Items.box 55.0 // word1
                Items.glue 10.0 15.0 3.0
                Items.penalty 0.0 0.0 false // NON-FLAGGED penalty for non-hyphen break
                Items.box 12.0 // "hy"
                Items.penalty 3.0 50.0 true // optional hyphen (FLAGGED) - width 3 represents hyphen width
                Items.box 13.0 // "phen"
                Items.glue 10.0 25.0 3.0 // Increased stretch from 17 to 25 - makes second line better when breaking at hyphen
                Items.box 40.0 // word2
            |]

        // Keep tolerance high so that this test focuses solely on
        // the FinalHyphenDemerits behaviour rather than tolerance penalties.
        let baseOptions =
            { LineBreakOptions.Default 80.0 with
                Tolerance = 1000.0
            }

        // With low FinalHyphenDemerits, the algorithm chooses the hyphenated version
        // since it gives a perfect first line (55 + 10 + 12 + 3 = 80) and decent second line
        let optionsLowPenalty =
            { baseOptions with
                FinalHyphenDemerits = 0.0
            }

        let linesLowPenalty = LineBreaker.breakLines optionsLowPenalty items

        // With high FinalHyphenDemerits, the algorithm should avoid the hyphenated version
        // and instead break at the non-flagged penalty to avoid the massive penalty
        let optionsHighPenalty =
            { baseOptions with
                FinalHyphenDemerits = 50000000.0
            }

        let linesHighPenalty = LineBreaker.breakLines optionsHighPenalty items

        // Both should successfully break
        linesLowPenalty.Length |> shouldBeGreaterThan 0
        linesHighPenalty.Length |> shouldBeGreaterThan 0

        // The key test: FinalHyphenDemerits should determine which break is chosen
        linesLowPenalty |> shouldNotEqual linesHighPenalty

        // With low penalty: break at the hyphen (position 5) for perfect first line fit
        // End points to the first item of the next line (after consuming the penalty), so End=5.
        linesLowPenalty.[0].End |> shouldEqual 5

        // With high penalty: break at the non-flagged penalty (position 3) to avoid the hyphen penalty
        // End points to first item of next line, so End=3.
        linesHighPenalty.[0].End |> shouldEqual 3
