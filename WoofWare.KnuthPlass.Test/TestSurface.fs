namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open ApiSurface

[<TestFixture>]
module TestSurface =
    let assembly = typeof<Box>.Assembly

    [<Test>]
    let ``Ensure API surface has not been modified`` () = ApiSurface.assertIdentical assembly

    [<Test ; Explicit "not published yet">]
    // https://github.com/nunit/nunit3-vs-adapter/issues/876
    let CheckVersionAgainstRemote () =
        MonotonicVersion.validate assembly "WoofWare.KnuthPlass"

    [<Test ; Explicit>]
    let ``Update API surface`` () =
        ApiSurface.writeAssemblyBaseline assembly
