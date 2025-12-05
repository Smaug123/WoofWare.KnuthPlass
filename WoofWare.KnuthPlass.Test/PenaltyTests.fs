namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module PenaltyTests =
    [<Test>]
    let ``Penalty affects break choice`` () =
        // TeX's demerits formula includes the penalty cost (tex.web:16901-16908).
        // A high penalty at one break point should cause the algorithm to prefer
        // a different break point, even if the alternative has slightly worse geometry.
        //
        // Setup: Two potential break points with different penalty costs.
        // - Position 2 (after glue): No explicit penalty, geometry slightly worse
        // - Position 4 (high penalty): Very expensive penalty = 1000
        //
        // With line width 55, breaking at position 2 gives a loose line (needs stretching),
        // while breaking at position 4 gives a tighter line but incurs the penalty.
        // The algorithm should avoid the high-penalty break despite better geometry.
        let items =
            [|
                Items.box 30.0
                Items.glue 10.0 20.0 3.0 // Position 2: break after glue, no explicit penalty
                Items.box 20.0
                Items.penalty 0.0 5000.0 false // Position 4: high penalty
                Items.glue 10.0 5.0 3.0
                Items.box 30.0
            |]

        let options =
            { LineBreakOptions.Default 55.0 with
                Tolerance = 5000.0
            }

        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldEqual 2
        // The algorithm should avoid the high-penalty break at position 4,
        // preferring to break at position 2 despite looser geometry
        lines.[0].End |> shouldEqual 2

    [<Test>]
    let ``Flagged penalties incur double hyphen demerits`` () =
        // DoubleHyphenDemerits (tex.web:16906-16908) adds extra cost when two consecutive
        // lines both end at flagged penalties (like hyphens). This discourages having
        // multiple hyphenated lines in a row, which looks poor typographically.
        //
        // Setup: Three lines with two break options per position:
        // - Flagged penalty (like a hyphen)
        // - Unflagged penalty (like a regular word boundary)
        //
        // With low DoubleHyphenDemerits, consecutive flagged breaks may be chosen.
        // With high DoubleHyphenDemerits, the algorithm avoids consecutive flagged breaks.
        let items =
            [|
                Items.box 35.0
                Items.penalty 0.0 0.0 false // Unflagged option A
                Items.penalty 5.0 50.0 true // Flagged option A
                Items.glue 10.0 10.0 3.0
                Items.box 35.0
                Items.penalty 0.0 0.0 false // Unflagged option B
                Items.penalty 5.0 50.0 true // Flagged option B
                Items.glue 10.0 10.0 3.0
                Items.box 35.0
            |]

        let lowPenalty =
            { LineBreakOptions.Default 55.0 with
                DoubleHyphenDemerits = 0.0
                Tolerance = 5000.0
            }

        let highPenalty =
            { LineBreakOptions.Default 55.0 with
                DoubleHyphenDemerits = 10_000_000.0
                Tolerance = 5000.0
            }

        let linesLow = LineBreaker.breakLines lowPenalty items
        let linesHigh = LineBreaker.breakLines highPenalty items

        // Both should produce lines
        linesLow.Length |> shouldBeGreaterThan 0
        linesHigh.Length |> shouldBeGreaterThan 0

        // With high DoubleHyphenDemerits, at least one break should differ to avoid
        // consecutive flagged breaks. The specific positions depend on the geometry,
        // but the layouts should differ.
        linesLow |> shouldNotEqual linesHigh

    [<Test>]
    let ``Negative penalties should reduce demerits and encourage breaks`` () =
        // This test verifies that negative penalties correctly encourage breaks per TeX semantics.
        // TeX demerits formula (tex.web 16901-16908):
        // - Base: (line_penalty + badness)²
        // - Positive penalty P ≥ 0: base + P²
        // - Negative penalty P < 0: base - P²
        //
        // Setup: Create two competing FEASIBLE breaks where the penalty sign determines choice.
        // - Break A (at first penalty with cost 0): Looser line (higher badness), no penalty cost
        // - Break B (at second penalty with signed cost): Tighter line (lower badness), with signed penalty
        //
        // For Break A: demerits = (10 + 100)² = 12100
        // For Break B with negative penalty: (10 + 12.5)² - 150² = 506.25 - 22500 = -21994 → wins!
        // For Break B with positive penalty: (10 + 12.5)² + 150² = 506.25 + 22500 = 23006 → Break A wins!

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

        // Use penalty 150 which is high enough to flip the choice
        // For positive penalty, Break A (12100) < Break B (23006), so Break A wins
        // For negative penalty, Break B (-21994) < Break A (12100), so Break B wins
        let linesWithNegative = LineBreaker.breakLines options (makeItems -150.0)
        let linesWithPositive = LineBreaker.breakLines options (makeItems 150.0)

        // Both should successfully break
        linesWithNegative.Length |> shouldBeGreaterThan 0
        linesWithPositive.Length |> shouldBeGreaterThan 0

        // The negative penalty should encourage breaking at the second penalty (End=6)
        // because base - 150² provides strongly negative demerits.
        linesWithNegative.[0].End |> shouldEqual 6

        // The positive penalty should discourage breaking at the second penalty,
        // preferring the first zero-cost penalty (End=4) because base + 150² is very high.
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
