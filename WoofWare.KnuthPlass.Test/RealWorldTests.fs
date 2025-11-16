namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module RealWorldTests =
    [<Test>]
    let ``Format a typical paragraph`` () =
        let text =
            "The Knuth-Plass algorithm is a sophisticated line breaking algorithm used in TeX."

        let wordWidth (s : string) = float s.Length * 8.0
        let spaceWidth = 4.0
        let items = Items.fromString text wordWidth spaceWidth
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
        let items = Items.fromString text wordWidth spaceWidth

        let narrowLines = LineBreaker.breakLines (LineBreakOptions.Default 150.0) items
        let wideLines = LineBreaker.breakLines (LineBreakOptions.Default 500.0) items

        narrowLines.Length |> shouldBeGreaterThan wideLines.Length
