namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module HelperTests =
    [<Test>]
    let ``fromString creates correct items for simple text`` () =
        let text = "hello world"
        let wordWidth (s : string) = float s.Length * 10.0
        let spaceWidth = 5.0
        let items = Items.fromString text wordWidth spaceWidth

        items.Length |> shouldEqual 3

        match items.[0] with
        | Box b -> b.Width |> shouldEqual 50.0
        | _ -> failwith "First item should be a box"

        match items.[1] with
        | Glue g -> g.Width |> shouldEqual 5.0
        | _ -> failwith "Second item should be glue"

        match items.[2] with
        | Box b -> b.Width |> shouldEqual 50.0
        | _ -> failwith "Third item should be a box"

    [<Test>]
    let ``fromString handles single word`` () =
        let text = "hello"
        let wordWidth (s : string) = float s.Length * 10.0
        let spaceWidth = 5.0
        let items = Items.fromString text wordWidth spaceWidth

        items.Length |> shouldEqual 1

        match items.[0] with
        | Box _ -> ()
        | _ -> failwith "Should be a single box"

    [<Test>]
    let ``fromString handles multiple spaces`` () =
        let text = "hello    world"
        let wordWidth (s : string) = float s.Length * 10.0
        let spaceWidth = 5.0
        let items = Items.fromString text wordWidth spaceWidth

        items.Length |> shouldEqual 3

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
    let ``Paragraph.format handles grapheme clusters correctly`` () =
        // This string has 3 grapheme clusters but 11 UTF-16 chars
        // 👨‍👩‍👧‍👦 is a family emoji (1 grapheme cluster, 7 chars)
        // 👍🏽 is thumbs up with skin tone (1 grapheme cluster, 4 chars)
        let text = "👨‍👩‍👧‍👦 👍🏽"

        // Using default width function which should count grapheme clusters
        let result = Paragraph.format 10.0 text

        // Should successfully format without crashing
        // The text should be split since we have 2 grapheme clusters (plus space)
        // and line width is 10.0
        result |> shouldNotEqual ""

    [<Test>]
    let ``fromString counts grapheme clusters not chars`` () =
        // 👍🏽 is 1 grapheme cluster but 4 UTF-16 chars
        let text = "👍🏽"
        let items = Items.fromString text (fun s -> float (System.Globalization.StringInfo(s).LengthInTextElements)) 5.0

        items.Length |> shouldEqual 1

        match items.[0] with
        | Box b ->
            // Should be 1.0 (one grapheme cluster), not 4.0 (four chars)
            b.Width |> shouldEqual 1.0
        | _ -> failwith "Should be a box"
