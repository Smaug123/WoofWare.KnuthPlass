namespace WoofWare.KnuthPlass.Test

open System.Threading
open NUnit.Framework
open WoofWare.KnuthPlass
open WoofWare.LiangHyphenation
open WoofWare.Expect

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module LiangHyphenationTests =
    /// Liang hyphenation trie for British English.
    let private enGbTrie =
        System.Lazy<_> ((fun () -> LanguageData.load KnownLanguage.EnGb), LazyThreadSafetyMode.ExecutionAndPublication)

    /// Hyphenation function using Liang algorithm with en-GB data.
    let liangHyphenate (word : string) : byte array =
        Hyphenation.hyphenate enGbTrie.Value word

    [<OneTimeSetUp>]
    let setUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let tearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    [<Test>]
    let ``format produces formatted text with Liang hyphenation`` () =
        let text = "The quick brown fox jumps over the lazy dog."
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
                    liangHyphenate
                    text
        }

    [<Test>]
    let ``format produces formatted text with Liang hyphenation, monospace`` () =
        let text = "The quick brown fox jumps over the lazy dog."
        let lineWidth = 20.0f

        expect {
            snapshot
                @"The quick brown fox
jumps over the lazy
dog."

            return
                Text.format
                    (LineBreakOptions.DefaultMonospace lineWidth)
                    Text.defaultWordWidth
                    Items.monospaceGlue
                    Hyphenation.DEFAULT_PENALTY
                    liangHyphenate
                    text
        }

    [<Test>]
    let ``formatParagraph on Jekyll and Hyde with Liang hyphenation`` () =
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
alone, to mortify a taste for vintages; and though he enjoyed the theatre, had
not crossed the doors of one for twenty years. But he had an approved tolerance
for others; sometimes wondering, almost with envy, at the high pressure of
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
                    Hyphenation.DEFAULT_PENALTY
                    liangHyphenate
                    text
        }

    [<Test>]
    let ``formatParagraph on Puck speech with Liang hyphenation`` () =
        let text =
            Assembly.readEmbeddedResource "publicdomain.puck_speech.txt"
            |> fun s -> s.Replace("\r", "").Replace ("\n", " ")

        expect {
            snapshot
                @"If we shadows have offended, Think but this, and all is mended, That you have
but slumber’d here While these visions did appear. And this weak and idle theme,
No more yielding but a dream, Gentles, do not reprehend. If you pardon, we will
mend. And, as I am an honest Puck, If we have unearnèd luck Now to ’scape the
serpent’s tongue, We will make amends ere long; Else the Puck a liar call. So,
good night unto you all. Give me your hands, if we be friends, And Robin shall
restore amends."

            return
                Text.format
                    (LineBreakOptions.DefaultMonospace 80.0f)
                    Text.defaultWordWidth
                    Items.monospaceGlue
                    Hyphenation.DEFAULT_PENALTY
                    liangHyphenate
                    text
        }

    [<Test>]
    let ``narrow column with Liang hyphenation`` () =
        let text = "extraordinary circumstances necessitate unconventional solutions"
        let lineWidth = 15.0f

        expect {
            snapshot
                @"extraordinary
circumstances
necessitate un-
conventional
solutions"

            return
                Text.format
                    (LineBreakOptions.DefaultMonospace lineWidth)
                    Text.defaultWordWidth
                    Items.monospaceGlue
                    Hyphenation.DEFAULT_PENALTY
                    liangHyphenate
                    text
        }

    [<Test>]
    let ``very narrow column forces hyphenation`` () =
        let text = "internationalization"
        let lineWidth = 10.0f

        expect {
            snapshot
                @"interna-
tionaliza-
tion"

            return
                Text.format
                    (LineBreakOptions.DefaultMonospace lineWidth)
                    Text.defaultWordWidth
                    Items.monospaceGlue
                    Hyphenation.DEFAULT_PENALTY
                    liangHyphenate
                    text
        }

    [<Test>]
    let ``hyphenation respects word boundaries`` () =
        let text = "The internationalization of software requires careful consideration."
        let lineWidth = 30.0f

        expect {
            snapshot
                @"The internationalization of
software requires careful con-
sideration."

            return
                Text.format
                    (LineBreakOptions.DefaultMonospace lineWidth)
                    Text.defaultWordWidth
                    Items.monospaceGlue
                    Hyphenation.DEFAULT_PENALTY
                    liangHyphenate
                    text
        }
