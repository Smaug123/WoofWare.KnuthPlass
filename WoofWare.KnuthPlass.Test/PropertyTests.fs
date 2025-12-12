namespace WoofWare.KnuthPlass.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.KnuthPlass

[<TestFixture>]
module PropertyTests =

    /// Compute the actual width of a line, excluding trailing glue.
    let private computeLineWidth (items : Item[]) (startIdx : int) (endIdx : int) : float32 =
        let mutable width = 0.0f

        for i = startIdx to endIdx - 1 do
            match items.[i] with
            | Box b -> width <- width + b.Width
            | Glue g -> width <- width + g.Width
            | Penalty _ -> ()

        // Exclude trailing glue
        if endIdx > 0 && endIdx <= items.Length then
            match items.[endIdx - 1] with
            | Glue g -> width <- width - g.Width
            | Penalty p -> width <- width + p.Width // Penalty width (e.g., hyphen) IS included
            | _ -> ()

        width

    /// Compute the shrink available on a line, excluding trailing glue.
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

    /// Check if a line is overfull (width exceeds target even with maximum shrink).
    let private isOverfull (items : Item[]) (lineWidth : float32) (startIdx : int) (endIdx : int) : bool =
        let width = computeLineWidth items startIdx endIdx
        let shrink = computeLineShrink items startIdx endIdx
        let minPossibleWidth = width - shrink
        minPossibleWidth > lineWidth + 1e-6f

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

    // ============================================================================
    // Properties that would have caught the rescue-node demerits bug
    // ============================================================================

    /// Get all legal breakpoints in an items array.
    let private getAllLegalBreakpoints (items : Item[]) : int list =
        [ 0 .. items.Length ] |> List.filter (isLegalBreakpoint items)

    /// Compute the stretch available on a line, excluding trailing glue.
    let private computeLineStretch (items : Item[]) (startIdx : int) (endIdx : int) : float32 =
        let mutable stretch = 0.0f

        for i = startIdx to endIdx - 1 do
            match items.[i] with
            | Glue g -> stretch <- stretch + g.Stretch
            | _ -> ()

        // Exclude trailing glue
        if endIdx > 0 && endIdx <= items.Length then
            match items.[endIdx - 1] with
            | Glue g -> stretch <- stretch - g.Stretch
            | _ -> ()

        stretch

    /// Compute the adjustment ratio for a line.
    let private computeRatio (items : Item[]) (lineWidth : float32) (startIdx : int) (endIdx : int) : float32 option =
        let width = computeLineWidth items startIdx endIdx
        let diff = lineWidth - width

        if abs diff < 1e-6f then
            Some 0.0f
        elif diff > 0.0f then
            let stretch = computeLineStretch items startIdx endIdx

            if stretch > 0.0f then Some (diff / stretch) else None // Underfull with no stretch
        else
            let shrink = computeLineShrink items startIdx endIdx

            if shrink > 0.0f then Some (diff / shrink) else None // Overfull with no shrink

    /// Compute badness from ratio (TeX formula: 100 * |ratio|^3, capped at 10000).
    let private badness (ratio : float32) : float32 =
        min (100.0f * (abs ratio ** 3.0f)) 10000.0f

    /// Check if a line is feasible (ratio >= -1 and badness <= tolerance).
    let private isLineFeasible
        (items : Item[])
        (lineWidth : float32)
        (tolerance : float32)
        (startIdx : int)
        (endIdx : int)
        : bool
        =
        match computeRatio items lineWidth startIdx endIdx with
        | None -> false
        | Some ratio -> ratio >= -1.0f && badness ratio <= tolerance

    /// Try to find a feasible breaking of items into lines using exhaustive search.
    /// Returns Some breaks if a feasible solution exists, None otherwise.
    /// A feasible solution has all lines with ratio >= -1 and badness <= tolerance.
    let rec private tryFindFeasibleBreaking
        (items : Item[])
        (lineWidth : float32)
        (tolerance : float32)
        (legalBreakpoints : int list)
        (currentStart : int)
        : int list option
        =
        if currentStart >= items.Length then
            // Reached the end - success!
            Some []
        else
            // Try each legal breakpoint after currentStart
            let candidateEnds =
                legalBreakpoints
                |> List.filter (fun bp -> bp > currentStart && bp <= items.Length)

            candidateEnds
            |> List.tryPick (fun endPos ->
                if isLineFeasible items lineWidth tolerance currentStart endPos then
                    // This line is feasible, try to complete the rest
                    match tryFindFeasibleBreaking items lineWidth tolerance legalBreakpoints endPos with
                    | Some restBreaks -> Some (endPos :: restBreaks)
                    | None -> None
                else
                    None
            )

    /// Check if a feasible solution exists for breaking items into lines.
    let private feasibleSolutionExists (items : Item[]) (lineWidth : float32) (tolerance : float32) : bool =
        let legalBreakpoints = getAllLegalBreakpoints items

        tryFindFeasibleBreaking items lineWidth tolerance legalBreakpoints 0
        |> Option.isSome

    /// Property: If the algorithm produces an overfull line, no feasible alternative should exist.
    /// This is the core property violated by the rescue-node demerits bug.
    [<Test>]
    let ``No overfull line when a feasible alternative exists`` () =
        let property (text : string) (lineWidth : float32) =
            let items = Items.fromEnglishString Text.defaultWordWidth Text.SPACE_WIDTH text

            if items.Length = 0 then
                true
            else
                let options = LineBreakOptions.Default lineWidth
                let lines = LineBreaker.breakLines options items

                // Check if any line is overfull
                let hasOverfullLine =
                    lines
                    |> Array.exists (fun line -> isOverfull items lineWidth line.Start line.End)

                if hasOverfullLine then
                    // If we have an overfull line, verify no feasible solution exists
                    let feasibleExists = feasibleSolutionExists items lineWidth options.Tolerance

                    if feasibleExists then
                        failwithf
                            "Algorithm produced overfull line but feasible solution exists! Text: %s, LineWidth: %f"
                            text
                            lineWidth

                    not feasibleExists
                else
                    true

        let arb =
            ArbMap.defaults
            |> ArbMap.generate<string * float32>
            |> Gen.zip genEnglishLikeText
            |> Gen.zip genLineWidth
            |> Gen.map (fun (lineWidth, (text, _)) -> text, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (text, lineWidth) -> property text lineWidth)

        Check.One (FsCheckConfig.config, prop)

    // ============================================================================
    // Reference implementation for small inputs
    // ============================================================================

    /// Compute demerits for a single line (simplified version of TeX formula).
    let private lineDemerits (ratio : float32) (linePenalty : float32) : float32 =
        let bad = badness ratio
        let d = linePenalty + bad
        d * d

    /// Exhaustively find the optimal breaking by trying all valid combinations.
    /// Returns (totalDemerits, breakpoints) for the best solution found.
    let rec private findOptimalBreakingExhaustive
        (items : Item[])
        (lineWidth : float32)
        (tolerance : float32)
        (linePenalty : float32)
        (legalBreakpoints : int list)
        (currentStart : int)
        (accDemerits : float32)
        : (float32 * int list) option
        =
        if currentStart >= items.Length then
            Some (accDemerits, [])
        else
            let candidateEnds =
                legalBreakpoints
                |> List.filter (fun bp -> bp > currentStart && bp <= items.Length)

            let solutions =
                candidateEnds
                |> List.choose (fun endPos ->
                    match computeRatio items lineWidth currentStart endPos with
                    | Some ratio when ratio >= -1.0f && badness ratio <= tolerance ->
                        let thisLineDemerits = lineDemerits ratio linePenalty

                        match
                            findOptimalBreakingExhaustive
                                items
                                lineWidth
                                tolerance
                                linePenalty
                                legalBreakpoints
                                endPos
                                (accDemerits + thisLineDemerits)
                        with
                        | Some (totalDem, breaks) -> Some (totalDem, endPos :: breaks)
                        | None -> None
                    | _ -> None
                )

            if List.isEmpty solutions then
                None
            else
                solutions |> List.minBy fst |> Some

    /// Generate small item arrays for exhaustive testing.
    let private genSmallItems : Gen<Item[]> =
        let genBox = Gen.choose (1, 20) |> Gen.map (float32 >> Items.box)

        let genGlue =
            Gen.map3
                (fun w s sh -> Items.glue (float32 w) (float32 s) (float32 sh))
                (Gen.choose (1, 10))
                (Gen.choose (0, 5))
                (Gen.choose (0, 3))

        // Generate alternating box-glue patterns (like words with spaces)
        Gen.choose (1, 4)
        |> Gen.bind (fun wordCount ->
            let genWord = genBox

            Gen.listOfLength wordCount genWord
            |> Gen.map (fun boxes ->
                boxes
                |> List.collect (fun box -> [ box ; Items.glue 2.0f 1.0f 1.0f ])
                |> List.take (boxes.Length * 2 - 1) // Remove trailing glue
                |> List.toArray
            )
        )

    /// Property: For small inputs, the algorithm's result should match exhaustive search.
    [<Test>]
    let ``Algorithm matches reference implementation on small inputs`` () =
        let property (items : Item[]) (lineWidth : float32) =
            if items.Length = 0 then
                true
            else
                let options = LineBreakOptions.Default lineWidth
                let legalBreakpoints = getAllLegalBreakpoints items

                // Find optimal solution exhaustively
                let exhaustiveResult =
                    findOptimalBreakingExhaustive
                        items
                        lineWidth
                        options.Tolerance
                        options.LinePenalty
                        legalBreakpoints
                        0
                        0.0f

                match exhaustiveResult with
                | None ->
                    // No feasible solution exists - algorithm should still produce something (rescue)
                    let lines = LineBreaker.breakLines options items
                    lines.Length > 0
                | Some (optimalDemerits, optimalBreaks) ->
                    // Compare with algorithm result
                    let lines = LineBreaker.breakLines options items
                    let algorithmBreaks = lines |> Array.map (fun l -> l.End) |> Array.toList

                    // The algorithm should find a solution with demerits close to optimal
                    // (allowing small floating point differences)
                    let algorithmDemerits =
                        let mutable total = 0.0f
                        let mutable start = 0

                        for line in lines do
                            match computeRatio items lineWidth start line.End with
                            | Some ratio ->
                                total <- total + lineDemerits ratio options.LinePenalty
                                start <- line.End
                            | None ->
                                // Overfull/underfull line - use inf_bad
                                total <- total + lineDemerits 10000.0f options.LinePenalty
                                start <- line.End

                        total

                    // Algorithm should not be significantly worse than optimal
                    // (Note: our simplified demerits don't include all TeX factors like fitness class,
                    // so we allow some slack)
                    if algorithmDemerits > optimalDemerits * 1.5f + 1000.0f then
                        failwithf
                            "Algorithm demerits (%f) significantly worse than optimal (%f). Algorithm breaks: %A, Optimal breaks: %A"
                            algorithmDemerits
                            optimalDemerits
                            algorithmBreaks
                            optimalBreaks

                    true

        let arb =
            Gen.zip genSmallItems (Gen.choose (15, 60) |> Gen.map float32) |> Arb.fromGen

        let prop = Prop.forAll arb (fun (items, lineWidth) -> property items lineWidth)

        Check.One (FsCheckConfig.config.WithMaxTest 1000, prop)

    // ============================================================================
    // Direct test that rescue nodes are only used when necessary
    // ============================================================================

    /// Property: If all lines in the result are feasible, the result should not contain
    /// any line that could have been avoided by choosing a different feasible break.
    [<Test>]
    let ``Rescue mechanism is only used when no feasible solution exists`` () =
        let property (text : string) (lineWidth : float32) =
            let items = Items.fromEnglishString Text.defaultWordWidth Text.SPACE_WIDTH text

            if items.Length = 0 then
                true
            else
                let options = LineBreakOptions.Default lineWidth
                let lines = LineBreaker.breakLines options items

                // Count overfull lines in the result
                let overfullCount =
                    lines
                    |> Array.filter (fun line -> isOverfull items lineWidth line.Start line.End)
                    |> Array.length

                if overfullCount > 0 then
                    // Verify that we couldn't have done better
                    let feasibleExists = feasibleSolutionExists items lineWidth options.Tolerance

                    feasibleExists |> shouldEqual false
                    not feasibleExists
                else
                    true

        let arb =
            ArbMap.defaults
            |> ArbMap.generate<string * float32>
            |> Gen.zip genEnglishLikeText
            |> Gen.zip genLineWidth
            |> Gen.map (fun (lineWidth, (text, _)) -> text, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (text, lineWidth) -> property text lineWidth)

        Check.One (FsCheckConfig.config, prop)
