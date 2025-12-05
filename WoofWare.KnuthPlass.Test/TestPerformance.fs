namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module TestPerformance =
    [<Test>]
    let ``Performance: Should handle large paragraphs`` () =
        let word = Items.box 5.0f
        let space = Items.glue 1.0f 0.5f 0.2f

        // Create 5000 words separated by spaces
        let items = Array.init 10000 (fun i -> if i % 2 = 0 then word else space)

        let options = LineBreakOptions.Default 100.0f

        let stopwatch = System.Diagnostics.Stopwatch.StartNew ()

        let lines = LineBreaker.breakLines options items

        stopwatch.Stop ()

        // An O(N^2) implementation should choke on 10_000 items.
        // On my laptop this takes 41ms; surely a GitHub free runner can't take more than 20x the time.
        stopwatch.ElapsedMilliseconds |> shouldBeSmallerThan 1000L

        lines.Length |> shouldBeGreaterThan 1
