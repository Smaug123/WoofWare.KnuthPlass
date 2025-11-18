namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module TestPerformance =
    [<Test ; Explicit>]
    let ``Performance: Should handle large paragraphs linearly`` () =
        // Generate a very long paragraph (e.g., 2000 words)
        // With N=2000, N^2 = 4,000,000 iterations.
        // In a complex logic loop, this might take 500ms-1s.
        // With N=5000, N^2 = 25,000,000. This will definitely hang a slow implementation.

        let word = Items.box 5.0
        let space = Items.glue 1.0 0.5 0.2

        // Create 5000 words separated by spaces
        let items = Array.init 10000 (fun i -> if i % 2 = 0 then word else space)

        let options = LineBreakOptions.Default 100.0

        let stopwatch = System.Diagnostics.Stopwatch.StartNew ()

        let lines = LineBreaker.breakLines options items

        stopwatch.Stop ()

        printfn "Time taken: %d ms" stopwatch.ElapsedMilliseconds

        // An O(N^2) implementation usually chokes on 10,000 items (taking several seconds).
        // An O(N) implementation should do this in under 200ms easily.
        // Adjust threshold based on your machine, but 1000ms is a very generous upper bound for O(N).
        stopwatch.ElapsedMilliseconds |> shouldBeSmallerThan 1000L

        lines.Length |> shouldBeGreaterThan 0
