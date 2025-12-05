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
    let ``Text.format processes emoji text without crashing`` () =
        // This is a basic smoke test for emoji/Unicode handling.
        // The actual grapheme cluster counting is tested in ``fromString counts grapheme clusters not chars``.
        let text = "👨‍👩‍👧‍👦 👍🏽"

        // Using default width function
        let result = Text.formatEnglishFixedWidth 10.0 text

        // Should successfully format without crashing
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
    let ``Paragraph ends with finishing glue and forced break`` () =
        // Per Knuth-Plass paper: paragraphs end with finishing glue and a forced break.
        //
        // Note: In TeX proper, the paragraph tail uses par_fill_skip preceded by an
        // infinite positive penalty (so it's not a breakpoint), and the forced final
        // break is injected by the line-breaker via try_break(eject_penalty) rather
        // than appearing as a node in the item list (tex.web:16070, 17197).
        //
        // This library's Items.fromEnglishString uses a simpler representation:
        // finishing glue (width=0, stretch=∞) followed by an explicit forced break
        // penalty (cost=-∞). This achieves the same effect for our algorithm.
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
    let ``fromString treats newlines as paragraph boundaries`` () =
        // Note: This is a library design choice, NOT TeX behavior.
        // In TeX, a single newline is converted to a space; only a blank line
        // or explicit \par creates a paragraph break.
        //
        // This library's Items.fromEnglishString treats each newline as a
        // paragraph boundary, producing separate paragraph items for each line.
        // This is convenient for plain-text input where newlines are meaningful.
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
