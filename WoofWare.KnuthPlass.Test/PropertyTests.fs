namespace WoofWare.KnuthPlass.Test

open System.Threading
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.KnuthPlass

[<TestFixture>]
module PropertyTests =

    /// High-level specification for a paragraph of items.
    /// This is what we actually generate; it compiles down to Item[].
    /// The structure guarantees validity by construction: every segment has a box,
    /// and we always append proper termination.
    module ParagraphSpec =
        type PenaltySpec =
            {
                Width : float32
                Cost : float32
                Flagged : bool
            }

        type GlueSpec =
            {
                Width : float32
                Stretch : float32
                Shrink : float32
            }

        /// A segment is: Box, then zero or more Penalties, then Glue.
        /// This mirrors the structure "word [hyphenation-points] space".
        type SegmentSpec =
            {
                BoxWidth : float32
                Penalties : PenaltySpec list
                Glue : GlueSpec
            }

        /// A paragraph is a non-empty list of segments.
        /// We use a head + tail representation to guarantee non-emptiness by construction.
        type T =
            {
                Head : SegmentSpec
                Tail : SegmentSpec list
            }

            member this.Segments = this.Head :: this.Tail

        /// Compile the high-level spec to an Item array.
        let compile (spec : T) : Item[] =
            [|
                for seg in spec.Segments do
                    yield
                        Item.Box
                            {
                                Width = seg.BoxWidth
                            }

                    for pen in seg.Penalties do
                        yield
                            Item.Penalty
                                {
                                    Width = pen.Width
                                    Cost = pen.Cost
                                    Flagged = pen.Flagged
                                }

                    yield
                        Item.Glue
                            {
                                Width = seg.Glue.Width
                                Stretch = seg.Glue.Stretch
                                Shrink = seg.Glue.Shrink
                            }

                // Termination: finishing glue (infinite stretch) + forced break
                yield
                    Item.Glue
                        {
                            Width = 0.0f
                            Stretch = infinityf
                            Shrink = 0.0f
                        }
                yield
                    Item.Penalty
                        {
                            Width = 0.0f
                            Cost = -infinityf
                            Flagged = false
                        }
            |]

    /// Generators for ParagraphSpec, parameterized by penalty probability.
    module ParagraphGen =
        open ParagraphSpec

        let genPenaltySpec : Gen<PenaltySpec> =
            gen {
                // Width: typically small (hyphen width), but allow 0
                let! width = Gen.choose (0, 5) |> Gen.map float32
                // Cost: can be negative (encourage break) or positive (discourage)
                // Avoid infinity here; that's for forced/forbidden breaks
                let! cost = Gen.choose (-100, 100) |> Gen.map float32
                let! flagged = Gen.elements [ true ; false ]

                return
                    {
                        Width = width
                        Cost = cost
                        Flagged = flagged
                    }
            }

        let genGlueSpec : Gen<GlueSpec> =
            gen {
                // Natural width: positive, representing space between words
                let! width = Gen.choose (1, 10) |> Gen.map float32
                // Stretch: how much it can grow (0 = rigid)
                let! stretch = Gen.choose (0, 5) |> Gen.map float32
                // Shrink: how much it can compress (0 = rigid)
                let! shrink = Gen.choose (0, 3) |> Gen.map float32

                return
                    {
                        Width = width
                        Stretch = stretch
                        Shrink = shrink
                    }
            }

        /// Generate a segment with penalties appearing with the given probability.
        /// Uses geometric distribution with a cap to prevent unbounded generation.
        let genSegmentSpec (penaltyProbability : float) : Gen<SegmentSpec> =
            let maxPenalties = 10 // Cap to prevent runaway generation

            let rec genPenalties (acc : PenaltySpec list) : Gen<PenaltySpec list> =
                gen {
                    if List.length acc >= maxPenalties then
                        return List.rev acc
                    else
                        let! roll = Gen.choose (0, 99)
                        let addMore = float roll < penaltyProbability * 100.0

                        if addMore then
                            let! pen = genPenaltySpec
                            return! genPenalties (pen :: acc)
                        else
                            return List.rev acc
                }

            gen {
                // Box width: positive, representing word/content width
                let! boxWidth = Gen.choose (1, 20) |> Gen.map float32
                let! penalties = genPenalties []
                let! glue = genGlueSpec

                return
                    {
                        BoxWidth = boxWidth
                        Penalties = penalties
                        Glue = glue
                    }
            }

        /// Generate a paragraph spec with the given penalty probability.
        let genParagraphSpec (penaltyProbability : float) : Gen<T> =
            gen {
                let! segmentCount = Gen.choose (0, 19) // 0-19 tail segments = 1-20 total
                let! head = genSegmentSpec penaltyProbability
                let! tail = Gen.listOfLength segmentCount (genSegmentSpec penaltyProbability)

                return
                    {
                        Head = head
                        Tail = tail
                    }
            }

        /// Generate a (penaltyProbability, paragraphSpec, lineWidth) triple.
        /// Line width is correlated with content to ensure interesting test scenarios:
        /// - Low multipliers (30-70%) produce tight/overfull lines
        /// - High multipliers (100-150%) produce loose/underfull lines
        let genTestCase : Gen<float * T * float32> =
            gen {
                let! penaltyProb = Gen.choose (0, 100) |> Gen.map (fun x -> float x / 100.0)
                let! spec = genParagraphSpec penaltyProb

                // Compute content metrics
                let totalBoxWidth = spec.Segments |> List.sumBy (fun s -> s.BoxWidth)
                let avgSegmentWidth = totalBoxWidth / float32 (List.length spec.Segments)

                // Generate line width as 30%-150% of average segment width
                // This ensures meaningful relationship between content and line width
                let! multiplier = Gen.choose (30, 150) |> Gen.map (fun x -> float32 x / 100.0f)
                let lineWidth = max 1.0f (avgSegmentWidth * multiplier)

                return (penaltyProb, spec, lineWidth)
            }

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
            |> Gen.zip genEnglishLikeText
            |> Gen.zip genLineWidth
            |> Gen.map (fun (lineWidth, (text, _)) -> text, lineWidth)
            |> Arb.fromGen

        let prop = Prop.forAll arb (fun (text, lineWidth) -> property text lineWidth)

        Check.One (FsCheckConfig.config, prop)

    /// Precomputed cumulative sums for O(1) line width/shrink queries
    type private CumulativeSums =
        {
            /// cumWidth[i] = sum of (box + glue widths) for items 0..i-1
            Width : float32[]
            /// cumShrink[i] = sum of glue shrinks for items 0..i-1
            Shrink : float32[]
        }

    let private computeCumulativeSums (items : Item[]) : CumulativeSums =
        let n = items.Length
        let cumWidth = Array.zeroCreate (n + 1)
        let cumShrink = Array.zeroCreate (n + 1)

        for i = 0 to n - 1 do
            match items.[i] with
            | Box b ->
                cumWidth.[i + 1] <- cumWidth.[i] + b.Width
                cumShrink.[i + 1] <- cumShrink.[i]
            | Glue g ->
                cumWidth.[i + 1] <- cumWidth.[i] + g.Width
                cumShrink.[i + 1] <- cumShrink.[i] + g.Shrink
            | Penalty _ ->
                cumWidth.[i + 1] <- cumWidth.[i]
                cumShrink.[i + 1] <- cumShrink.[i]

        {
            Width = cumWidth
            Shrink = cumShrink
        }

    /// Check if a line is overfull using precomputed sums. O(1) per query.
    let private isOverfullFast
        (items : Item[])
        (sums : CumulativeSums)
        (lineWidth : float32)
        (startIdx : int)
        (endIdx : int)
        : bool
        =
        // Base width from cumulative sums
        let mutable width = sums.Width.[endIdx] - sums.Width.[startIdx]
        let mutable shrink = sums.Shrink.[endIdx] - sums.Shrink.[startIdx]

        // Adjust for trailing item: exclude trailing glue width/shrink, include penalty width
        if endIdx > 0 && endIdx <= items.Length then
            match items.[endIdx - 1] with
            | Glue g ->
                width <- width - g.Width
                shrink <- shrink - g.Shrink
            | Penalty p -> width <- width + p.Width
            | _ -> ()

        let minPossibleWidth = width - shrink
        minPossibleWidth > lineWidth + 1e-6f

    /// Check if a line is overfull (width exceeds target even with maximum shrink).
    /// O(n) per call - use isOverfullFast with precomputed sums for bulk queries.
    let private isOverfull (items : Item[]) (lineWidth : float32) (startIdx : int) (endIdx : int) : bool =
        let sums = computeCumulativeSums items
        isOverfullFast items sums lineWidth startIdx endIdx

    /// Check if there exists a feasible solution using memoized search with precomputed sums.
    /// Complexity: O(n²) with O(n) space for memoization and cumulative sums.
    let private existsFeasibleSolution (items : Item[]) (lineWidth : float32) (startPos : int) : bool =
        let sums = computeCumulativeSums items
        let memo = System.Collections.Generic.Dictionary<int, bool> ()

        let rec search (currentPos : int) : bool =
            if currentPos >= items.Length then
                true
            else
                match memo.TryGetValue currentPos with
                | true, result -> result
                | false, _ ->
                    // Try all possible next break positions
                    let mutable found = false
                    let mutable nextPos = currentPos + 1

                    while not found && nextPos <= items.Length do
                        if LineBreaker.isValidBreakpoint items nextPos then
                            // Check if this line would be feasible (not overfull)
                            if not (isOverfullFast items sums lineWidth currentPos nextPos) then
                                // Recursively check if the rest can be broken feasibly
                                if search nextPos then
                                    found <- true

                        nextPos <- nextPos + 1

                    memo.[currentPos] <- found
                    found

        search startPos

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

    [<Test>]
    let ``No overfull line when a feasible alternative exists - arbitrary items`` () =
        let mutable overfullCount = 0
        let mutable fineCount = 0

        let property (penaltyProb : float) (spec : ParagraphSpec.T) (lineWidth : float32) =
            let items = ParagraphSpec.compile spec

            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items

            // Check each line for overfullness
            for line in lines do
                if not (isOverfull items lineWidth line.Start line.End) then
                    Interlocked.Increment &fineCount |> ignore<int>
                else

                // This line is overfull - check if a feasible solution existed
                Interlocked.Increment &overfullCount |> ignore<int>

                if existsFeasibleSolution items lineWidth 0 then
                    failwithf
                        "Overfull line from %d to %d when feasible solution exists. PenaltyProb: %f, LineWidth: %f, Segments: %d, Items: %d"
                        line.Start
                        line.End
                        penaltyProb
                        lineWidth
                        (List.length spec.Segments)
                        items.Length

        let arb = Arb.fromGen ParagraphGen.genTestCase

        let prop =
            Prop.forAll arb (fun (penaltyProb, spec, lineWidth) -> property penaltyProb spec lineWidth)

        Check.One (FsCheckConfig.config, prop)

        // Assert that the test was actually testing something (some lines were overfull)
        float overfullCount / float (overfullCount + fineCount)
        |> shouldBeGreaterThan 0.1
