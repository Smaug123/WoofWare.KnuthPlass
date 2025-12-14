namespace WoofWare.KnuthPlass.Test

open FsCheck
open FsCheck.FSharp
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
                            $"Illegal breakpoint at position %d{line.End} (items.Length=%d{items.Length}). Item before: %A{itemBefore}, Item two before: %A{itemTwoBefore}"

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

    /// Helper: compute line width excluding trailing glue, including penalty width
    let private computeLineWidth (items : Item[]) (startIdx : int) (endIdx : int) : float32 =
        let mutable width = 0.0f

        for i = startIdx to endIdx - 1 do
            match items.[i] with
            | Box b -> width <- width + b.Width
            | Glue g -> width <- width + g.Width
            | Penalty _ -> ()

        // Exclude trailing glue, but include penalty width (for hyphens)
        if endIdx > 0 && endIdx <= items.Length then
            match items.[endIdx - 1] with
            | Glue g -> width <- width - g.Width
            | Penalty p -> width <- width + p.Width
            | _ -> ()

        width

    /// Helper: compute shrink available on a line, excluding trailing glue
    let private computeLineShrink (items : Item[]) (startIdx : int) (endIdx : int) : float32 =
        let mutable shrink = 0.0f

        for i = startIdx to endIdx - 1 do
            match items.[i] with
            | Glue g -> shrink <- shrink + g.Shrink
            | _ -> ()

        // Exclude trailing glue
        if endIdx > 0 && endIdx <= items.Length then
            match items.[endIdx - 1] with
            | Glue g -> shrink <- shrink - g.Shrink
            | _ -> ()

        shrink

    /// Check if a line is overfull (width exceeds target even with maximum shrink)
    let private isOverfull (items : Item[]) (lineWidth : float32) (startIdx : int) (endIdx : int) : bool =
        let width = computeLineWidth items startIdx endIdx
        let shrink = computeLineShrink items startIdx endIdx
        let minPossibleWidth = width - shrink
        minPossibleWidth > lineWidth + 1e-6f

    /// Check if there exists a feasible solution using exhaustive search
    let rec private existsFeasibleSolution (items : Item[]) (lineWidth : float32) (currentPos : int) : bool =
        if currentPos >= items.Length then
            true
        else
            // Try all possible next break positions
            let mutable found = false
            let mutable nextPos = currentPos + 1

            while not found && nextPos <= items.Length do
                if isLegalBreakpoint items nextPos then
                    // Check if this line would be feasible (not overfull)
                    if not (isOverfull items lineWidth currentPos nextPos) then
                        // Recursively check if the rest can be broken feasibly
                        if existsFeasibleSolution items lineWidth nextPos then
                            found <- true

                nextPos <- nextPos + 1

            found

    [<Test>]
    let ``No overfull line when a feasible alternative exists`` () =
        let property (text : string) (lineWidth : float32) =
            let items = Items.fromEnglishString Text.defaultWordWidth Text.SPACE_WIDTH text

            if items.Length = 0 then
                // Empty text trivially passes
                ()
            else
                let options = LineBreakOptions.Default lineWidth
                let lines = LineBreaker.breakLines options items

                // Check each line for overfullness
                for line in lines do
                    if isOverfull items lineWidth line.Start line.End then
                        // This line is overfull - check if a feasible solution existed
                        if existsFeasibleSolution items lineWidth 0 then
                            failwithf
                                "Overfull line from %d to %d when feasible solution exists. Text: %s, LineWidth: %f"
                                line.Start
                                line.End
                                text
                                lineWidth

        let arb =
            ArbMap.defaults
            |> ArbMap.generate<string * float32>
            |> Gen.zip genEnglishLikeText
            |> Gen.zip genLineWidth
            |> Gen.map (fun (lineWidth, (text, _)) -> text, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (text, lineWidth) -> property text lineWidth)

        Check.One (FsCheckConfig.config, prop)
