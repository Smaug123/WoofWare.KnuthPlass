namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
// If we ever see this flake in CI even once, we'll disable the performance tests in CI
[<Category "Performance">]
[<NonParallelizable>]
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

    /// Ensure O(n) performance when items include a terminal forced break.
    /// The forcedBreakAhead optimization must not count the terminal Penalty(-inf),
    /// otherwise it prevents node deactivation and causes O(n²) behavior.
    [<Test>]
    let ``Performance: Terminal forced break does not cause O(n²)`` () =
        let words = Array.init 500 (fun i -> $"word{i}")
        let text = String.concat " " words

        let items = TestHelpers.fromStringNoHyphenation text
        let options = LineBreakOptions.Default 100.0f

        let stopwatch = System.Diagnostics.Stopwatch.StartNew ()
        let lines = LineBreaker.breakLines options items
        stopwatch.Stop ()

        // With O(n) algorithm, 500 words should take < 500ms
        // With O(n²) algorithm, it would take much longer
        stopwatch.ElapsedMilliseconds |> shouldBeSmallerThan 500L

        lines.Length |> shouldBeGreaterThan 1

    /// Verify large paragraphs with terminal penalty complete quickly.
    [<Test>]
    let ``Performance: Large paragraph with terminal penalty`` () =
        let word = Items.box 5.0f
        let space = Items.glue 1.0f 0.5f 0.2f

        let itemsList = ResizeArray<Item> ()

        for _ in 0..4999 do
            itemsList.Add word
            itemsList.Add space

        // Add terminal penalty (like Text.format does)
        itemsList.Add (Items.glue 0.0f System.Single.PositiveInfinity 0.0f)
        itemsList.Add (Items.forcedBreak ())

        let items = itemsList.ToArray ()
        let options = LineBreakOptions.Default 100.0f

        let stopwatch = System.Diagnostics.Stopwatch.StartNew ()
        let lines = LineBreaker.breakLines options items
        stopwatch.Stop ()

        // Should complete in reasonable time (< 2 seconds even on slow machines)
        // O(n²) on 10000 items would take much longer
        stopwatch.ElapsedMilliseconds |> shouldBeSmallerThan 2000L

        lines.Length |> shouldBeGreaterThan 1
