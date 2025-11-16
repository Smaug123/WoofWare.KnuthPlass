namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped
open WoofWare.Expect

[<TestFixture>]
module RealWorldTests =
    [<Test>]
    let ``Format a typical paragraph`` () =
        let text =
            "The Knuth-Plass algorithm is a sophisticated line breaking algorithm used in TeX."

        let wordWidth (s : string) = float s.Length * 8.0
        let spaceWidth = 4.0
        let items = Items.fromEnglishString wordWidth spaceWidth text
        let options = LineBreakOptions.Default 250.0
        let lines = LineBreaker.breakLines options items

        lines.Length |> shouldBeGreaterThan 0
        lines.[0].Start |> shouldEqual 0
        lines.[lines.Length - 1].End |> shouldEqual items.Length

    [<Test>]
    let ``Varying line widths`` () =
        let text = "This is a test of the line breaking algorithm with multiple words."
        let wordWidth (s : string) = float s.Length * 10.0
        let spaceWidth = 5.0
        let items = Items.fromEnglishString wordWidth spaceWidth text

        let narrowLines = LineBreaker.breakLines (LineBreakOptions.Default 150.0) items
        let wideLines = LineBreaker.breakLines (LineBreakOptions.Default 500.0) items

        narrowLines.Length |> shouldBeGreaterThan wideLines.Length

    [<Test>]
    let ``formatParagraph' produces formatted text`` () =
        let text = "The quick brown fox jumps over the lazy dog."
        let wordWidth (s : string) = float s.Length * 8.0
        let spaceWidth = 4.0
        let lineWidth = 80.0

        expect {
            snapshot
                @"The quick
brown fox
jumps over
the lazy
dog."

            return Paragraph.format lineWidth wordWidth spaceWidth Hyphenation.DEFAULT_PENALTY Hyphenation.none text
        }

    [<Test>]
    let ``formatParagraph produces formatted text`` () =
        let text = "The quick brown fox jumps over the lazy dog."
        let wordWidth (s : string) = float s.Length * 8.0
        let spaceWidth = 4.0
        let lineWidth = 80.0

        expect {
            snapshot
                @"The quick
brown fox
jumps over
the lazy
dog."

            return Paragraph.format lineWidth wordWidth spaceWidth Hyphenation.DEFAULT_PENALTY Hyphenation.none text
        }

    [<Test>]
    let ``formatParagraph on Jekyll and Hyde`` () =
        let text =
            Assembly.readEmbeddedResource "publicdomain.jekyll_and_hyde.txt"
            |> fun s -> s.Replace("\r", "").Replace ("\n", " ")

        expect {
            snapshot
                @"Mr. Utterson the lawyer was a man of a rugged countenance that was never lighted
by a smile; cold, scanty and embarrassed in discourse; backward in sentiment;
lean, long, dusty, dreary and yet somehow lovable. At friendly meetings, and
when the wine was to his taste, something eminently human beaconed from his eye;
something indeed which never found its way into his talk, but which spoke not
only in these silent symbols of the after-dinner face, but more often and loudly
in the acts of his life. He was austere with himself; drank gin when he was
alone, to mortify a taste for vintages; and though he enjoyed the theatre, had not
crossed the doors of one for twenty years. But he had an approved tolerance for
others; sometimes wondering, almost with envy, at the high pressure of spirits
involved in their misdeeds; and in any extremity inclined to help rather than
to reprove. “I incline to Cain’s heresy,” he used to say quaintly: “I let my
brother go to the devil in his own way.” In this character, it was frequently
his fortune to be the last reputable acquaintance and the last good influence in
the lives of downgoing men. And to such as these, so long as they came about his
chambers, he never marked a shade of change in his demeanour."

            return Paragraph.formatEnglish 80.0 text
        }
