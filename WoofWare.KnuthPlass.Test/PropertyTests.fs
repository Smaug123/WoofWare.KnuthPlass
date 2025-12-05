namespace WoofWare.KnuthPlass.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.KnuthPlass

[<TestFixture>]
module PropertyTests =

    /// Checks whether a breakpoint at position `breakPos` in the items array is legal.
    /// A legal breakpoint is either:
    /// 1. At the end of the paragraph (breakPos = items.Length)
    /// 2. At a penalty with non-positive-infinite cost
    /// 3. At a glue that immediately follows a box
    let private isLegalBreakpoint (items : Item[]) (breakPos : int) : bool =
        if breakPos = items.Length then
            // End of paragraph is always legal
            true
        elif breakPos <= 0 || breakPos > items.Length then
            // Invalid positions
            false
        else
            match items.[breakPos - 1] with
            | Penalty p ->
                // Breaking at a penalty is legal if the penalty is not +infinity
                not (System.Single.IsPositiveInfinity p.Cost)
            | Glue _ ->
                // Breaking at a glue is legal if the preceding item is a box
                breakPos >= 2
                && match items.[breakPos - 2] with
                   | Box _ -> true
                   | _ -> false
            | Box _ ->
                // Cannot break immediately after a box (unless at a penalty or glue)
                false

    /// Generates a non-empty string of lowercase letters and spaces
    let private genEnglishLikeText : Gen<string> =
        Gen.choose (1, 20)
        |> Gen.bind (fun wordCount ->
            let genWord =
                Gen.choose (1, 10)
                |> Gen.bind (fun len ->
                    Gen.elements [ 'a' .. 'z' ]
                    |> Gen.listOfLength len
                    |> Gen.map (fun chars -> System.String (chars |> List.toArray))
                )

            Gen.listOfLength wordCount genWord |> Gen.map (String.concat " ")
        )

    /// Generates a positive line width that's reasonable for the text
    let private genLineWidth : Gen<float32> = Gen.choose (10, 200) |> Gen.map float32

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
                    let isLegal = isLegalBreakpoint items line.End

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
                            "Illegal breakpoint at position %d (items.Length=%d). Item before: %A, Item two before: %A"
                            line.End
                            items.Length
                            itemBefore
                            itemTwoBefore

                    isLegal
                )

        let arb =
            ArbMap.defaults
            |> ArbMap.generate<string * float32>
            |> Gen.zip genEnglishLikeText
            |> Gen.zip genLineWidth
            |> Gen.map (fun (lineWidth, (text, _)) -> text, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (text, lineWidth) -> property text lineWidth)

        Check.One (FsCheckConfig.config, prop)
