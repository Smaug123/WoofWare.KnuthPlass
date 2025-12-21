namespace WoofWare.KnuthPlass.Test

open System
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
    let ``box helper creates correct item`` () =
        let item = Items.box 42.0f

        match item with
        | Box b -> b.Width |> shouldEqual 42.0f
        | _ -> failwith "Should create a Box item"

    [<Test>]
    let ``glue helper creates correct item`` () =
        let item = Items.glue 10.0f 5.0f 3.0f

        match item with
        | Glue g ->
            g.Width |> shouldEqual 10.0f
            g.Stretch |> shouldEqual 5.0f
            g.Shrink |> shouldEqual 3.0f
        | _ -> failwith "Should create a Glue item"

    [<Test>]
    let ``penalty helper creates correct item`` () =
        let item = Items.penalty 2.0f 100.0f true

        match item with
        | Penalty p ->
            p.Width |> shouldEqual 2.0f
            p.Cost |> shouldEqual 100.0f
            p.Flagged |> shouldEqual true
        | _ -> failwith "Should create a Penalty item"

    [<Test>]
    let ``forcedBreak creates infinite negative penalty`` () =
        let item = Items.forcedBreak ()

        match item with
        | Penalty p ->
            p.Width |> shouldEqual LanguagePrimitives.GenericZero
            p.Cost |> shouldEqual Single.NegativeInfinity
            p.Flagged |> shouldEqual false
        | _ -> failwith "Should create a Penalty item"

    [<Test>]
    let ``wordFromFragments with single fragment creates single box`` () =
        let items =
            Items.wordFromFragments 1.0f (ReadOnlySpan [| 5.0f |]) (ReadOnlySpan [||])

        items.Length |> shouldEqual 1

        match items.[0] with
        | Box b -> b.Width |> shouldEqual 5.0f
        | _ -> failwith "Should create a Box item"

    [<Test>]
    let ``wordFromFragments with multiple fragments creates boxes and penalties`` () =
        let fragmentWidths = [| 3.0f ; 2.0f ; 4.0f |]
        let penalties = [| 50.0f ; 75.0f |]
        let hyphenWidth = 1.0f

        let items =
            Items.wordFromFragments hyphenWidth (ReadOnlySpan fragmentWidths) (ReadOnlySpan penalties)

        // Should be: Box, Penalty, Box, Penalty, Box = 5 items
        items.Length |> shouldEqual 5

        expect {
            snapshotList
                [
                    "box[3.00]"
                    "penF[1.00 cost 50.00]"
                    "box[2.00]"
                    "penF[1.00 cost 75.00]"
                    "box[4.00]"
                ]

            return items
        }

    [<Test>]
    let ``wordFromFragments validates penalty count`` () =
        let fragmentWidths = [| 3.0f ; 2.0f ; 4.0f |]
        let wrongPenalties = [| 50.0f |] // Should be 2, not 1

        (fun () ->
            Items.wordFromFragments 1.0f (ReadOnlySpan fragmentWidths) (ReadOnlySpan wrongPenalties)
            |> ignore
        )
        |> shouldFail<ArgumentException>

    [<Test>]
    let ``prioritiesToPoints converts Liang priorities correctly`` () =
        // Priorities: 0=no break, 1=break, 2=no break, 3=break, 4=no break
        // In standard Liang, odd values indicate valid hyphenation points (all treated equally)
        let priorities = [| 0uy ; 1uy ; 2uy ; 3uy ; 4uy |]
        let penalty = 10.0f

        let points = Hyphenation.prioritiesToPoints penalty priorities

        // Should have 2 points: position 2 (after 1uy) and position 4 (after 3uy)
        points.Length |> shouldEqual 2

        let struct (pos1, pen1) = points.[0]
        let struct (pos2, pen2) = points.[1]

        pos1 |> shouldEqual 2 // After index 1
        pen1 |> shouldEqual 10.0f // All odd values get the same penalty

        pos2 |> shouldEqual 4 // After index 3
        pen2 |> shouldEqual 10.0f // All odd values get the same penalty

    [<Test>]
    let ``prioritiesToPoints with no odd values returns empty`` () =
        let priorities = [| 0uy ; 2uy ; 4uy ; 6uy |]
        let points = Hyphenation.prioritiesToPoints 10.0f priorities

        points.Length |> shouldEqual 0

    [<Test>]
    let ``Text.format processes text with custom hyphenation`` () =
        let text = "hello world"
        let options = LineBreakOptions.Default 50.0f

        let result =
            Text.format
                options
                Text.defaultWordWidth
                (Items.defaultGlue Text.SPACE_WIDTH)
                50.0f
                TestHelpers.noHyphenation
                text

        // Should produce formatted text
        result |> shouldNotEqual ""

    [<Test>]
    let ``defaultGlue creates glue with expected stretch/shrink ratios`` () =
        let width = 10.0f
        let glue = Items.defaultGlue width

        glue.Width |> shouldEqual 10.0f
        glue.Stretch |> shouldEqual 5.0f // width * 0.5
        glue.Shrink |> shouldEqual (10.0f / 3.0f) // width / 3

    [<Test>]
    let ``monospaceGlue has width 1 and can't shrink`` () =
        let glue = Items.monospaceGlue

        glue.Width |> shouldEqual 1.0f
        glue.Stretch |> shouldEqual 0.5f
        glue.Shrink |> shouldEqual 0.0f

    [<Test>]
    let ``applyMinLengths zeros out positions for short word`` () =
        // Word "and" has length 3, priorities array has length 2
        // Position 0 = break after 'a' (1 char left, 2 right)
        // Position 1 = break after 'n' (2 chars left, 1 right)
        // With leftMin=2, rightMin=3: no valid positions
        let priorities = [| 1uy ; 1uy |]
        Hyphenation.applyMinLengths 2 3 3 priorities

        priorities |> shouldEqual [| 0uy ; 0uy |]

    [<Test>]
    let ``applyMinLengths preserves valid positions for longer word`` () =
        // Word "beautiful" has length 9, priorities array has length 8
        // With leftMin=2, rightMin=3:
        //   Position 0: 1 left, 8 right - INVALID (left < 2)
        //   Position 1: 2 left, 7 right - valid
        //   Position 2: 3 left, 6 right - valid
        //   Position 3: 4 left, 5 right - valid
        //   Position 4: 5 left, 4 right - valid
        //   Position 5: 6 left, 3 right - valid
        //   Position 6: 7 left, 2 right - INVALID (right < 3)
        //   Position 7: 8 left, 1 right - INVALID (right < 3)
        let priorities = [| 1uy ; 1uy ; 1uy ; 1uy ; 1uy ; 1uy ; 1uy ; 1uy |]
        Hyphenation.applyMinLengths 2 3 9 priorities

        priorities |> shouldEqual [| 0uy ; 1uy ; 1uy ; 1uy ; 1uy ; 1uy ; 0uy ; 0uy |]

    [<Test>]
    let ``applyMinLengths handles empty array`` () =
        let priorities : byte array = [||]
        Hyphenation.applyMinLengths 2 3 1 priorities

        priorities |> shouldEqual [||]

    [<Test>]
    let ``applyMinLengths with leftMin=1 rightMin=1 preserves all`` () =
        // With minimal constraints, all positions should remain
        let priorities = [| 1uy ; 3uy ; 5uy |]
        Hyphenation.applyMinLengths 1 1 4 priorities

        priorities |> shouldEqual [| 1uy ; 3uy ; 5uy |]

    [<Test>]
    let ``applyMinLengths default constants match TeX English`` () =
        Hyphenation.DEFAULT_LEFT_MIN |> shouldEqual 2
        Hyphenation.DEFAULT_RIGHT_MIN |> shouldEqual 3
