namespace WoofWare.KnuthPlass.Test

open FsCheck

[<RequireQualifiedAccess>]
module FsCheckConfig =

    let config =
        Config.QuickThrowOnFailure
            .WithQuietOnSuccess(true)
            .WithMaxTest(10000)
            .WithParallelRunConfig (
                {
                    MaxDegreeOfParallelism = max (System.Environment.ProcessorCount / 2) 1
                }
            )
