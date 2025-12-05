namespace WoofWare.KnuthPlass.Benchmarks

open BenchmarkDotNet.Attributes
open WoofWare.KnuthPlass

/// <summary>
/// Benchmarks for the Knuth-Plass line breaking algorithm.
/// Tests performance with varying paragraph sizes to ensure linear time complexity.
/// </summary>
[<MemoryDiagnoser>]
[<InProcess>]
type LineBreakingBenchmarks () =

    /// Number of words in the paragraph (each word is represented by a Box + Glue pair)
    [<Params(1000, 5000, 10000, 50000)>]
    member val WordCount = 0 with get, set

    /// Line width for breaking
    [<DefaultValue>]
    val mutable lineWidth : float32

    /// Pre-generated items array for the benchmark
    [<DefaultValue>]
    val mutable items : Item array

    /// Line break options
    [<DefaultValue>]
    val mutable options : LineBreakOptions

    [<GlobalSetup>]
    member this.Setup () =
        this.lineWidth <- 100.0f

        // Create word and space items
        let word = Items.box 5.0f
        let space = Items.glue 1.0f 0.5f 0.2f

        // Create alternating word-space pattern
        // Total items = WordCount * 2 (word + space for each)
        this.items <- Array.init (this.WordCount * 2) (fun i -> if i % 2 = 0 then word else space)

        this.options <- LineBreakOptions.Default this.lineWidth

    /// <summary>
    /// Main benchmark: measures time to break lines in a paragraph.
    /// Should scale linearly with input size (O(N) complexity).
    /// </summary>
    [<Benchmark>]
    member this.BreakLines () =
        LineBreaker.breakLines this.options this.items

    /// <summary>
    /// Benchmark focusing on measuring allocation patterns.
    /// Useful for identifying memory optimization opportunities.
    /// </summary>
    [<Benchmark>]
    member this.BreakLinesMemoryProfile () =
        let lines = LineBreaker.breakLines this.options this.items
        lines.Length // Return length to prevent dead code elimination


/// <summary>
/// Benchmarks for different line widths to test how the algorithm
/// handles varying output constraints.
/// </summary>
[<MemoryDiagnoser>]
[<InProcess>]
type LineWidthVariationBenchmarks () =

    /// Line width variations
    [<Params(50.0, 100.0, 150.0, 200.0)>]
    member val LineWidth = 0.0 with get, set

    [<DefaultValue>]
    val mutable items : Item array

    [<DefaultValue>]
    val mutable options : LineBreakOptions

    [<GlobalSetup>]
    member this.Setup () =
        // Fixed paragraph size
        let word = Items.box 5.0f
        let space = Items.glue 1.0f 0.5f 0.2f
        this.items <- Array.init 20000 (fun i -> if i % 2 = 0 then word else space)
        this.options <- LineBreakOptions.Default (float32 this.LineWidth)

    [<Benchmark>]
    member this.BreakLines () =
        LineBreaker.breakLines this.options this.items


/// <summary>
/// Benchmarks for realistic text scenarios with varying item types.
/// </summary>
[<MemoryDiagnoser>]
[<InProcess>]
type RealWorldScenarioBenchmarks () =

    [<DefaultValue>]
    val mutable items : Item array

    [<DefaultValue>]
    val mutable options : LineBreakOptions

    [<GlobalSetup>]
    member this.Setup () =
        // Create a more realistic scenario with:
        // - Varying word widths (3.0 to 8.0)
        // - Normal spaces with stretch and shrink
        // - Occasional penalties (for hyphens)
        let rng = System.Random (42) // Fixed seed for reproducibility

        let items = ResizeArray<Item> ()

        for _ in 1..50000 do
            // Add word with random width
            let wordWidth = 3.0f + rng.NextSingle () * 5.0f
            items.Add (Items.box wordWidth)

            // 10% chance of a hyphenation point
            if rng.NextDouble () < 0.1 then
                items.Add (Items.penalty 0.0f 50.0f true) // Flagged penalty for hyphen
                items.Add (Items.box (rng.NextSingle () * 5.0f))

            // Add space
            items.Add (Items.glue 1.0f 0.5f 0.2f)

        this.items <- items.ToArray ()
        this.options <- LineBreakOptions.Default 100.0f

    [<Benchmark>]
    member this.BreakRealWorldParagraph () =
        LineBreaker.breakLines this.options this.items
