namespace WoofWare.KnuthPlass.Test

open FsUnitTyped
open NUnit.Framework
open FsCheck
open FsCheck.FSharp
open WoofWare.KnuthPlass

/// Properties that isValidBreakpoint should satisfy according to Knuth-Plass/TeX.
[<TestFixture>]
module IsValidBreakpointProperties =

    let private genItems : Gen<Item[]> =
        ParagraphGen.genTestCase
        |> Gen.map (fun (_, spec, _) -> ParagraphSpec.compile spec)

    // Property 1: Position 0 (start of paragraph) is NOT a valid breakpoint.
    // It's where we START, not where we BREAK. (TeX semantics)
    [<Test>]
    let ``Position 0 is not a valid breakpoint`` () =
        let property (items : Item[]) =
            LineBreaker.isValidBreakpoint items 0 |> shouldEqual false

        Check.One (FsCheckConfig.config, Prop.forAll (Arb.fromGen genItems) property)

    // Property 2: Position items.Length (end of paragraph) is always valid
    [<Test>]
    let ``Position at end of paragraph is always valid`` () =
        let property (items : Item[]) =
            LineBreaker.isValidBreakpoint items items.Length |> shouldEqual true

        Check.One (FsCheckConfig.config, Prop.forAll (Arb.fromGen genItems) property)

    // Property 3: Positions outside [0, items.Length] are invalid
    [<Test>]
    let ``Positions outside bounds are invalid`` () =
        let property (items : Item[]) (offset : int) =
            let negPos = -(abs offset + 1)
            let pastEnd = items.Length + abs offset + 1

            LineBreaker.isValidBreakpoint items negPos |> shouldEqual false
            LineBreaker.isValidBreakpoint items pastEnd |> shouldEqual false

        let arb = Gen.zip genItems (Gen.choose (0, 100)) |> Arb.fromGen

        Check.One (FsCheckConfig.config, Prop.forAll arb (fun (items, offset) -> property items offset))

    // Property 4: Breaking at a penalty with finite cost is valid
    [<Test>]
    let ``Breaking at penalty with finite cost is valid`` () =
        let property (items : Item[]) =
            for i = 1 to items.Length - 1 do
                match items.[i - 1] with
                | Penalty p when not (System.Single.IsPositiveInfinity p.Cost) ->
                    if not (LineBreaker.isValidBreakpoint items i) then
                        failwithf "Position %d after finite-cost penalty %A should be valid" i p
                | _ -> ()

        Check.One (FsCheckConfig.config, Prop.forAll (Arb.fromGen genItems) property)

    // Property 5: Breaking at a penalty with +infinity cost is invalid
    [<Test>]
    let ``Breaking at penalty with infinite cost is invalid`` () =
        // Construct items with a +infinity penalty
        let genWithInfinitePenalty =
            gen {
                let! items = genItems
                // Insert a +infinity penalty somewhere in the middle
                let! insertPos = Gen.choose (0, max 0 (items.Length - 1))

                let infinitePenalty =
                    Item.Penalty
                        {
                            Width = 0.0f
                            Cost = infinityf
                            Flagged = false
                        }

                let newItems =
                    [|
                        yield! items.[.. insertPos - 1]
                        yield infinitePenalty
                        yield! items.[insertPos..]
                    |]

                return (newItems, insertPos + 1)
            }

        let property (items : Item[], penaltyPos : int) =
            // The position after the +infinity penalty should be invalid
            // (unless it happens to be at the end, which is always valid)
            if penaltyPos < items.Length then
                LineBreaker.isValidBreakpoint items penaltyPos |> shouldEqual false

        Check.One (FsCheckConfig.config, Prop.forAll (Arb.fromGen genWithInfinitePenalty) property)

    // Property 6: Breaking at glue that follows a box is valid
    [<Test>]
    let ``Breaking at glue following box is valid`` () =
        let property (items : Item[]) =
            for i = 2 to items.Length - 1 do
                match items.[i - 1], items.[i - 2] with
                | Glue _, Box _ ->
                    if not (LineBreaker.isValidBreakpoint items i) then
                        failwithf "Position %d at glue following box should be valid" i
                | _ -> ()

        Check.One (FsCheckConfig.config, Prop.forAll (Arb.fromGen genItems) property)

    // Property 7: Breaking at glue that does NOT follow a box is invalid
    [<Test>]
    let ``Breaking at glue not following box is invalid`` () =
        let property (items : Item[]) =
            for i = 2 to items.Length - 1 do
                match items.[i - 1], items.[i - 2] with
                | Glue _, notBox when
                    not (
                        match notBox with
                        | Box _ -> true
                        | _ -> false
                    )
                    ->
                    if LineBreaker.isValidBreakpoint items i then
                        failwithf "Position %d at glue following %A (not box) should be invalid" i notBox
                | _ -> ()

        Check.One (FsCheckConfig.config, Prop.forAll (Arb.fromGen genItems) property)

    // Property 8: Breaking immediately after a box (no glue/penalty) is invalid
    [<Test>]
    let ``Breaking after box without glue or penalty is invalid`` () =
        let property (items : Item[]) =
            for i = 1 to items.Length - 1 do
                match items.[i - 1] with
                | Box _ ->
                    // Position i is right after a box - should be invalid
                    // (unless i = items.Length, which is always valid)
                    if i < items.Length && LineBreaker.isValidBreakpoint items i then
                        failwithf "Position %d immediately after box should be invalid" i
                | _ -> ()

        Check.One (FsCheckConfig.config, Prop.forAll (Arb.fromGen genItems) property)

    // Property 9: Position 1 in a 2-item array depends on items[0]
    // (Position 1 is NOT the end here, so end-of-paragraph rule doesn't apply)
    [<Test>]
    let ``Position 1 validity depends on first item (2-item array)`` () =
        let genItem =
            Gen.oneof
                [
                    Gen.map
                        (fun w ->
                            Item.Box
                                {
                                    Width = w
                                }
                        )
                        (Gen.choose (1, 10) |> Gen.map float32)
                    Gen.map
                        (fun (w, s, sh) ->
                            Item.Glue
                                {
                                    Width = w
                                    Stretch = s
                                    Shrink = sh
                                }
                        )
                        (Gen.zip3
                            (Gen.choose (1, 10) |> Gen.map float32)
                            (Gen.choose (0, 5) |> Gen.map float32)
                            (Gen.choose (0, 3) |> Gen.map float32))
                    Gen.map
                        (fun (w, c, f) ->
                            Item.Penalty
                                {
                                    Width = w
                                    Cost = c
                                    Flagged = f
                                }
                        )
                        (Gen.zip3
                            (Gen.choose (0, 5) |> Gen.map float32)
                            (Gen.choose (-100, 100) |> Gen.map float32)
                            (Gen.elements [ true ; false ]))
                ]

        let property (firstItem : Item) (secondItem : Item) =
            let items = [| firstItem ; secondItem |]

            // Position 1: breaking after items[0]
            match firstItem with
            | Penalty p when not (System.Single.IsPositiveInfinity p.Cost) ->
                // Finite penalty -> position 1 is valid
                LineBreaker.isValidBreakpoint items 1 |> shouldEqual true
            | Penalty _ ->
                // +infinity penalty -> position 1 is invalid (forbidden break)
                LineBreaker.isValidBreakpoint items 1 |> shouldEqual false
            | Glue _ ->
                // Glue at position 0 has no preceding box -> position 1 is invalid
                // (There's no items[-1] to be a Box)
                LineBreaker.isValidBreakpoint items 1 |> shouldEqual false
            | Box _ ->
                // Cannot break immediately after a box
                LineBreaker.isValidBreakpoint items 1 |> shouldEqual false

        let arb = Gen.zip genItem genItem |> Arb.fromGen

        Check.One (FsCheckConfig.config, Prop.forAll arb (fun (a, b) -> property a b))

    [<Test>]
    let ``All breakpoints in English text line breaking are legal`` () =
        let property (text : string) (lineWidth : float32) =
            let items = Items.fromEnglishString Text.defaultWordWidth Text.SPACE_WIDTH text

            if items.Length = 0 then
                // Empty text trivially passes
                true
            else
                // Use monospace defaults with high tolerance to ensure we can always find a breaking
                let options =
                    { LineBreakOptions.DefaultMonospace lineWidth with
                        Tolerance = 100000.0f
                    }

                let lines = LineBreaker.breakLines options items

                lines
                |> Array.forall (fun line ->
                    let isLegal = LineBreaker.isValidBreakpoint items line.End

                    if not isLegal then
                        let itemBefore =
                            if line.End > 0 && line.End <= items.Length then
                                Some items.[line.End - 1]
                            else
                                None

                        let itemTwoBefore =
                            if line.End > 1 && line.End <= items.Length then
                                Some items.[line.End - 2]
                            else
                                None

                        failwithf
                            $"Illegal breakpoint at position %d{line.End} (items.Length=%d{items.Length}). Item before: %A{itemBefore}, Item two before: %A{itemTwoBefore}"

                    isLegal
                )

        let arb =
            ArbMap.defaults
            |> ArbMap.generate<string * float32>
            |> Gen.zip EnglishGen.text
            |> Gen.zip (Gen.choose (10, 200) |> Gen.map float32)
            |> Gen.map (fun (lineWidth, (text, _)) -> text, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (text, lineWidth) -> property text lineWidth)

        Check.One (FsCheckConfig.config, prop)
