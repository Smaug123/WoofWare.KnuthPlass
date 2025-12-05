namespace WoofWare.KnuthPlass.Benchmarks

open BenchmarkDotNet.Running

module Program =

    [<EntryPoint>]
    let main args =
        let _ = BenchmarkRunner.Run<LineBreakingBenchmarks> ()
        let _ = BenchmarkRunner.Run<LineWidthVariationBenchmarks> ()
        let _ = BenchmarkRunner.Run<RealWorldScenarioBenchmarks> ()

        0
