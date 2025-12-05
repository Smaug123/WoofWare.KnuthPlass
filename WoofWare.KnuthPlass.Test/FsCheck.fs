namespace WoofWare.KnuthPlass.Test

open FsCheck

[<RequireQualifiedAccess>]
module FsCheckConfig =

    let config = Config.QuickThrowOnFailure.WithQuietOnSuccess(true).WithMaxTest(10000)
