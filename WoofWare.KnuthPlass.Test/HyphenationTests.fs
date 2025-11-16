namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module HyphenationTests =
    [<Test>]
    let ``everywhere allows hyphenation in 5-character word at position 2`` () =
        // This test catches an off-by-one bug where righthyphenmin=3 was
        // incorrectly enforced as 4 by using `word.Length - 4` instead of
        // `word.Length - 3`.
        //
        // For a 5-char word "hello":
        // - lefthyphenmin=2: at least 2 chars before hyphen
        // - righthyphenmin=3: at least 3 chars after hyphen
        // - Valid positions: 2 (he|llo, leaving 3 chars after)
        // - Buggy code: 2..1 = [] (empty)
        // - Fixed code: 2..2 = [2]

        let points = Hyphenation.everywhere "hello"
        points |> shouldEqual [ 2 ]

    [<Test>]
    let ``everywhere respects lefthyphenmin=2`` () =
        // Position 1 would leave only 1 char before hyphen (h|ello)
        // This should not be allowed
        let points = Hyphenation.everywhere "hello"
        points |> shouldNotContain 1

    [<Test>]
    let ``everywhere respects righthyphenmin=3`` () =
        // For 6-char word "helper":
        // - Position 2: he|lper (4 chars after) ✓
        // - Position 3: hel|per (3 chars after) ✓
        // - Position 4: help|er (2 chars after) ✗
        let points = Hyphenation.everywhere "helper"
        points |> shouldEqual [ 2 ; 3 ]

    [<Test>]
    let ``everywhere rejects words shorter than 5 chars`` () =
        // Words must be at least 5 chars to allow hyphenation
        Hyphenation.everywhere "word" |> shouldEqual []
        Hyphenation.everywhere "hi" |> shouldEqual []
        Hyphenation.everywhere "" |> shouldEqual []

    [<Test>]
    let ``simpleEnglish allows hyphenation in 5-character word with vowel pattern`` () =
        // Similar test for simpleEnglish, which also had the same bug
        // "hello" has vowels at positions 1 (e) and 4 (o)
        // Position 2 is after 'e' and before 'l' (consonant), so it should be allowed
        let points = Hyphenation.simpleEnglish "hello"

        // Should allow position 2: he|llo
        points |> List.contains 2 |> shouldEqual true

    [<Test>]
    let ``simpleEnglish respects minimum constraints`` () =
        // For "beautiful" (9 chars), vowels at positions 1(e), 2(a), 5(i), 7(u)
        // Valid breaks after vowels: positions 2,3,6,8 (all within 2..6 range)
        // Buggy code would use 2..5, missing position 6
        let points = Hyphenation.simpleEnglish "beautiful"

        // All points should be >= 2 (lefthyphenmin)
        points |> List.forall (fun p -> p >= 2) |> shouldEqual true

        // All points should be <= length - 3 (righthyphenmin=3)
        points |> List.forall (fun p -> p <= 9 - 3) |> shouldEqual true
