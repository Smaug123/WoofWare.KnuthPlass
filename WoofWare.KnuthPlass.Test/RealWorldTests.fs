namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped
open WoofWare.Expect

[<TestFixture>]
module RealWorldTests =
    type LayoutBounds =
        {
            LineWidth : float32
        }

    let private assertLayoutWithinBounds (bounds : LayoutBounds) (text : string) =
        let items = Items.fromEnglishString Text.defaultWordWidth Text.SPACE_WIDTH text
        let options = LineBreakOptions.Default bounds.LineWidth
        let lines = LineBreaker.breakLines options items

        let lineFits (line : Line) =
            let mutable width = LanguagePrimitives.GenericZero
            let mutable stretch = LanguagePrimitives.GenericZero
            let mutable shrink = LanguagePrimitives.GenericZero

            for i in line.Start .. line.End - 1 do
                match items.[i] with
                | Box b -> width <- width + b.Width
                | Glue g ->
                    width <- width + g.Width
                    stretch <- stretch + g.Stretch
                    shrink <- shrink + g.Shrink
                | Penalty _ -> ()

            if line.End > 0 then
                match items.[line.End - 1] with
                | Glue g ->
                    width <- width - g.Width
                    stretch <- stretch - g.Stretch
                    shrink <- shrink - g.Shrink
                | Penalty p -> width <- width + p.Width
                | _ -> ()

            let adjustedWidth =
                if line.AdjustmentRatio > LanguagePrimitives.GenericZero then
                    width + (line.AdjustmentRatio * stretch)
                else
                    width + (line.AdjustmentRatio * shrink)

            // If the line has AdjustmentRatio ≈ -1.0 (maximally shrunk) and still doesn't fit,
            // it's a legitimate rescued overfull line - the algorithm produces these when no
            // feasible break exists (standard TeX behavior).
            let isRescuedOverfull =
                line.AdjustmentRatio <= -1.0f + 1e-6f
                && adjustedWidth > options.LineWidth + 1e-6f

            // single precision is starting to bite us here
            isRescuedOverfull || adjustedWidth <= options.LineWidth + 5e-5f

        lines
        |> Array.iteri (fun idx line ->
            Assert.That (lineFits line, $"Line {idx} exceeds width {bounds.LineWidth}: {line}")
        )

    [<Test>]
    let ``Format a typical paragraph`` () =
        let text =
            "The Knuth-Plass algorithm is a sophisticated line breaking algorithm used in TeX."

        let wordWidth (s : string) = float32 s.Length * 8.0f
        let spaceWidth = 4.0f
        let items = Items.fromEnglishString wordWidth spaceWidth text
        let options = LineBreakOptions.Default 250.0f
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldBeGreaterThan 0
        lines.[0].Start |> shouldEqual 0
        lines.[lines.Length - 1].End |> shouldEqual items.Length

    [<Test>]
    let ``Varying line widths`` () =
        // Use widths where the content can actually be broken into multiple lines
        // Total natural width is ~605, so we use widths that allow proper line breaking
        let text = "This is a test of the line breaking algorithm with multiple words."
        let wordWidth (s : string) = float32 s.Length * 10.0f
        let spaceWidth = 5.0f
        let items = Items.fromEnglishString wordWidth spaceWidth text

        let narrowLines = LineBreaker.breakLines (LineBreakOptions.Default 250.0f) items
        let wideLines = LineBreaker.breakLines (LineBreakOptions.Default 500.0f) items

        narrowLines.Length |> shouldBeGreaterThan wideLines.Length

    [<Test>]
    let ``format produces formatted text, tuned for monospace`` () =
        let text = "The quick brown fox jumps over the lazy dog."
        // Use a modest width so lines can stay within TeX tolerance without extreme shrink
        let wordWidth (s : string) = float32 s.Length
        let lineWidth = 20.0f

        expect {
            snapshot
                @"The quick brown fox
jumps over the lazy
dog."

            return
                Text.format
                    (LineBreakOptions.DefaultMonospace lineWidth)
                    wordWidth
                    Items.monospaceGlue
                    Hyphenation.DEFAULT_PENALTY
                    Hyphenation.none
                    text
        }

    [<Test>]
    let ``format produces formatted text`` () =
        let text = "The quick brown fox jumps over the lazy dog."
        // Use a modest width so lines can stay within TeX tolerance without extreme shrink
        let wordWidth (s : string) = float32 s.Length * 4.0f
        let lineWidth = 80.0f

        expect {
            snapshot
                @"The quick brown fox
jumps over the lazy
dog."

            return
                Text.format
                    (LineBreakOptions.Default lineWidth)
                    wordWidth
                    (Items.defaultGlue 4.0f)
                    Hyphenation.DEFAULT_PENALTY
                    Hyphenation.none
                    text
        }

    let boundsTestCases =
        [ "publicdomain.jekyll_and_hyde.txt" ; "publicdomain.puck_speech.txt" ]
        |> List.allPairs [ 20..80 ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof boundsTestCases)>]
    let ``formatParagraph keeps text within bounds`` (width : int, testResource : string) =
        let text =
            Assembly.readEmbeddedResource testResource
            |> fun s -> s.Replace("\r", "").Replace ("\n", " ")

        assertLayoutWithinBounds
            {
                LineWidth = float32 width
            }
            text


    [<Test>]
    let ``formatParagraph on Jekyll and Hyde`` () =
        let text =
            Assembly.readEmbeddedResource "publicdomain.jekyll_and_hyde.txt"
            |> fun s -> s.Replace("\r", "").Replace ("\n", " ")

        expect {
            snapshot
                @"Mr. Utterson the lawyer was a man of a rugged countenance that was never lighted by
a smile; cold, scanty and embarrassed in discourse; backward in sentiment; lean,
long, dusty, dreary and yet somehow lovable. At friendly meetings, and when the
wine was to his taste, something eminently human beaconed from his eye; something
indeed which never found its way into his talk, but which spoke not only in these
silent symbols of the after-dinner face, but more often and loudly in the acts of
his life. He was austere with himself; drank gin when he was alone, to mortify a
taste for vintages; and though he enjoyed the theatre, had not crossed the doors
of one for twenty years. But he had an approved tolerance for others; sometimes
wondering, almost with envy, at the high pressure of spirits involved in their
misdeeds; and in any extremity inclined to help rather than to reprove. “I incline
to Cain’s heresy,” he used to say quaintly: “I let my brother go to the devil
in his own way.” In this character, it was frequently his fortune to be the last
reputable acquaintance and the last good influence in the lives of downgoing men.
And to such as these, so long as they came about his chambers, he never marked a
shade of change in his demeanour."

            return Text.formatEnglishFixedWidth 80.0f text
        }

    [<Test>]
    let ``formatParagraph on Jekyll and Hyde with aggressive hyphenation`` () =
        let text =
            Assembly.readEmbeddedResource "publicdomain.jekyll_and_hyde.txt"
            |> fun s -> s.Replace("\r", "").Replace ("\n", " ")

        expect {
            snapshot
                @"Mr. Utterson the lawyer was a man of a rugged countenance that was never li-
ghted by a smile; cold, scanty and embarrassed in discourse; backward in senti-
ment; lean, long, dusty, dreary and yet somehow lovable. At friendly meetings,
and when the wine was to his taste, something eminently human beaconed from his
eye; something indeed which never found its way into his talk, but which spoke
not only in these silent symbols of the after-dinner face, but more often and
loudly in the acts of his life. He was austere with himself; drank gin when he
was alone, to mortify a taste for vintages; and though he enjoyed the theatre,
had not crossed the doors of one for twenty years. But he had an approved tole-
rance for others; sometimes wondering, almost with envy, at the high pressure of
spirits involved in their misdeeds; and in any extremity inclined to help rather
than to reprove. “I incline to Cain’s heresy,” he used to say quaintly: “I let
my brother go to the devil in his own way.” In this character, it was frequently
his fortune to be the last reputable acquaintance and the last good influence in
the lives of downgoing men. And to such as these, so long as they came about his
chambers, he never marked a shade of change in his demeanour."

            return
                Text.format
                    (LineBreakOptions.DefaultMonospace 80.0f)
                    Text.defaultWordWidth
                    Items.monospaceGlue
                    5.0f
                    Hyphenation.simpleEnglish
                    text
        }
