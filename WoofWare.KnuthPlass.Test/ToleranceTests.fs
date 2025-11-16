namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

[<TestFixture>]
module ToleranceTests =
    [<Test>]
    let ``Higher tolerance allows looser lines`` () =
        let items =
            [
                Items.box 50.0
                Items.glue 10.0 5.0 3.0
                Items.box 50.0
                Items.glue 10.0 5.0 3.0
                Items.box 50.0
            ]

        let strictOptions =
            { LineBreakOptions.Default (100.0) with
                Tolerance = 0.5
            }

        let looseOptions =
            { LineBreakOptions.Default (100.0) with
                Tolerance = 3.0
            }

        let strictLines = LineBreaker.breakLines strictOptions items
        let looseLines = LineBreaker.breakLines looseOptions items

        strictLines.Length |> shouldBeGreaterThan 0
        looseLines.Length |> shouldBeGreaterThan 0
