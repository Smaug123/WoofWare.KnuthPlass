namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.Expect
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module HelperTests =
    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    [<Test>]
    let ``fromString creates correct items for simple text`` () =
        let text = "hello world"
        let wordWidth (s : string) = float s.Length * 10.0
        let spaceWidth = 5.0
        let items = Items.fromEnglishString wordWidth spaceWidth text

        expect {
            snapshotList
                [
                    "box[20.00]"
                    "penF[10.00 cost 50.00]"
                    "box[30.00]"
                    "glue[5.00 / 2.50 / 1.67]"
                    "box[20.00]"
                    "penF[10.00 cost 50.00]"
                    "box[30.00]"
                    "glue[0.00 / Infinity / 0.00]"
                    "pen_[0.00 cost -Infinity]"
                ]

            return items
        }

    [<Test>]
    let ``fromString handles single word`` () =
        let text = "hello"
        let wordWidth (s : string) = float s.Length * 10.0
        let spaceWidth = 5.0
        let items = Items.fromEnglishString wordWidth spaceWidth text

        expect {
            snapshotList
                [
                    "box[20.00]"
                    "penF[10.00 cost 50.00]"
                    "box[30.00]"
                    "glue[0.00 / Infinity / 0.00]"
                    "pen_[0.00 cost -Infinity]"
                ]

            return items
        }

    [<Test>]
    let ``fromString concatenates multiple spaces`` () =
        let text = "hello    world"
        let wordWidth (s : string) = float s.Length * 10.0
        let spaceWidth = 5.0
        let items = Items.fromEnglishString wordWidth spaceWidth text

        expect {
            snapshotList
                [
                    "box[20.00]"
                    "penF[10.00 cost 50.00]"
                    "box[30.00]"
                    "glue[5.00 / 2.50 / 1.67]"
                    "box[20.00]"
                    "penF[10.00 cost 50.00]"
                    "box[30.00]"
                    "glue[0.00 / Infinity / 0.00]"
                    "pen_[0.00 cost -Infinity]"
                ]

            return items
        }

    [<Test>]
    let ``box helper creates correct item`` () =
        let item = Items.box 42.0

        match item with
        | Box b -> b.Width |> shouldEqual 42.0
        | _ -> failwith "Should create a Box item"

    [<Test>]
    let ``glue helper creates correct item`` () =
        let item = Items.glue 10.0 5.0 3.0

        match item with
        | Glue g ->
            g.Width |> shouldEqual 10.0
            g.Stretch |> shouldEqual 5.0
            g.Shrink |> shouldEqual 3.0
        | _ -> failwith "Should create a Glue item"

    [<Test>]
    let ``penalty helper creates correct item`` () =
        let item = Items.penalty 2.0 100.0 true

        match item with
        | Penalty p ->
            p.Width |> shouldEqual 2.0
            p.Cost |> shouldEqual 100.0
            p.Flagged |> shouldEqual true
        | _ -> failwith "Should create a Penalty item"

    [<Test>]
    let ``forcedBreak creates infinite negative penalty`` () =
        let item = Items.forcedBreak ()

        match item with
        | Penalty p ->
            p.Width |> shouldEqual 0.0
            p.Cost |> shouldEqual -infinity
            p.Flagged |> shouldEqual false
        | _ -> failwith "Should create a Penalty item"

    [<Test>]
    let ``Text.format handles grapheme clusters correctly`` () =
        // This string has 3 grapheme clusters but 11 UTF-16 chars
        // 👨‍👩‍👧‍👦 is a family emoji (1 grapheme cluster, 7 chars)
        // 👍🏽 is thumbs up with skin tone (1 grapheme cluster, 4 chars)
        let text = "👨‍👩‍👧‍👦 👍🏽"

        // Using default width function which should count grapheme clusters
        let result = Text.formatEnglish 10.0 text

        // Should successfully format without crashing
        // The text should be split since we have 2 grapheme clusters (plus space)
        // and line width is 10.0
        result |> shouldNotEqual ""

    [<Test>]
    let ``fromString counts grapheme clusters not chars`` () =
        // 👍🏽 is 1 grapheme cluster but 4 UTF-16 chars
        let text = "👍🏽"

        let items =
            Items.fromEnglishString (fun s -> float (System.Globalization.StringInfo(s).LengthInTextElements)) 5.0 text

        // The main content of the test is that we see 1.0 rather than 4.0 in the box width.
        expect {
            snapshotList [ "box[1.00]" ; "glue[0.00 / Infinity / 0.00]" ; "pen_[0.00 cost -Infinity]" ]
            return items
        }

    [<Test>]
    let ``Paragraph ends with TeX-compliant finishing glue and forced break`` () =
        // Per Knuth-Plass paper: "At the very end of a paragraph, two items are appended
        // so that the final line will be treated properly. First comes a glue item that
        // specifies the white space allowable at the right of the last line; then a penalty
        // item with penalty -infinity to force a break."
        // TeX uses finishing glue with width=0, stretch=infinity (fil), shrink=0.
        let text = "word"
        let items = Items.fromEnglishString (fun s -> float s.Length) 1.0 text

        // Last two items should be the finishing glue and forced break
        let finishingGlue = items.[items.Length - 2]
        let forcedBreak = items.[items.Length - 1]

        match finishingGlue with
        | Glue g ->
            g.Width |> shouldEqual 0.0
            System.Double.IsPositiveInfinity g.Stretch |> shouldEqual true
            g.Shrink |> shouldEqual 0.0
        | _ -> failwith "Second-to-last item should be finishing glue"

        match forcedBreak with
        | Penalty p ->
            p.Width |> shouldEqual 0.0
            System.Double.IsNegativeInfinity p.Cost |> shouldEqual true
            p.Flagged |> shouldEqual false
        | _ -> failwith "Last item should be forced break penalty"

    [<Test>]
    let ``fromString treats newlines as hard breaks`` () =
        let text = "hello\nworld"
        let wordWidth (s : string) = float s.Length * 10.0
        let spaceWidth = 5.0
        let items = Items.fromEnglishString wordWidth spaceWidth text

        expect {
            snapshotList
                [
                    "box[20.00]"
                    "penF[10.00 cost 50.00]"
                    "box[30.00]"
                    "glue[0.00 / Infinity / 0.00]"
                    "pen_[0.00 cost -Infinity]"
                    "box[20.00]"
                    "penF[10.00 cost 50.00]"
                    "box[30.00]"
                    "glue[0.00 / Infinity / 0.00]"
                    "pen_[0.00 cost -Infinity]"
                ]

            return items
        }
