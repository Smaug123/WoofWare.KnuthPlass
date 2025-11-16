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
