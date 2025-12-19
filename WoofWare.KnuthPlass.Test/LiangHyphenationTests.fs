namespace WoofWare.KnuthPlass.Test

open System
open System.IO
open System.Reflection
open System.Threading
open FsUnitTyped
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

    let columnWidths =
        [ 20 ; 40 ; 60 ; 80 ]

    let texts =
        typeof<Assembly.Dummy>.Assembly.GetManifestResourceNames()
        |> Array.choose (fun n ->
            let prefix = "WoofWare.KnuthPlass.Test.publicdomain."
            if n.StartsWith (prefix, StringComparison.OrdinalIgnoreCase) then
                let suffix = ".txt"
                assert (n.EndsWith (suffix, StringComparison.OrdinalIgnoreCase))
                n.Substring(prefix.Length, n.Length - prefix.Length - 4)
                |> Some
            else None
        )

    let snapshots =
        Seq.allPairs columnWidths texts
        |> Seq.map TestCaseData
        |> List.ofSeq

    [<TestCaseSource (nameof snapshots)>]
    let ``formatParagraph on texts with Liang hyphenation`` (width : int, resourceName : string) =
        let expected =
            Assembly.readEmbeddedResource $"hyphenated.%s{resourceName}.monospace.%i{width}.txt"
        let text =
            Assembly.readEmbeddedResource $"publicdomain.%s{resourceName}.txt"
            |> fun s -> s.Replace("\r", "").Replace ("\n", " ")

        let width = float32 width
        let actual =
            Text.format
                (LineBreakOptions.DefaultMonospace width)
                Text.defaultWordWidth
                Items.monospaceGlue
                Hyphenation.DEFAULT_PENALTY
                liangHyphenate
                text
        actual
        |> shouldEqual expected

    let findFolderContaining (dirName : string) =
        let rec go (current : DirectoryInfo) =
            if isNull current then
                failwith "failed to find test folder"
            let target = Path.Combine (current.FullName, dirName)
            if Directory.Exists target then
                DirectoryInfo target
            else
                go current.Parent

        go (FileInfo(Assembly.GetExecutingAssembly().Location).Directory)

    [<TestCaseSource (nameof snapshots)>]
    [<Explicit "Run this test explicitly to update the snapshots">]
    let ``update snapshots`` (width : int, resourceName : string) =
        let text =
            Assembly.readEmbeddedResource $"publicdomain.%s{resourceName}.txt"
            |> fun s -> s.Replace("\r", "").Replace ("\n", " ")

        let file =
            findFolderContaining "hyphenated"
            |> fun d -> Path.Combine (d.FullName, resourceName, "monospace", $"%i{width}.txt")
            |> FileInfo

        file.Directory.Create ()

        let width = float32 width
        let actual =
            Text.format
                (LineBreakOptions.DefaultMonospace width)
                Text.defaultWordWidth
                Items.monospaceGlue
                Hyphenation.DEFAULT_PENALTY
                liangHyphenate
                text

        File.WriteAllText (file.FullName, actual)

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
