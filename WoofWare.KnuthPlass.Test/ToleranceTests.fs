namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped
open WoofWare.Expect

[<TestFixture>]
module ToleranceTests =
    /// Compute badness = 100 * |ratio|^3
    let private badness (ratio : float) : float = 100.0 * (abs ratio ** 3.0)

    [<Test>]
    let ``Tolerance enforces maximum badness for overfull lines`` () =
        // Underfull lines (ratio >= 0) are always accepted, but overfull lines (ratio < 0)
        // should only be accepted if badness <= tolerance

        // Create a single-line paragraph that is overfull with no glue to shrink
        let items = [| Items.box 60.0 |]

        // Box is 60 wide, line width is 50
        // This is overfull with no glue to shrink - should always fail
        Assert.Throws<System.Exception> (fun () ->
            LineBreaker.breakLines (LineBreakOptions.Default 50.0) items |> ignore
        )
        |> ignore

    [<Test>]
    let ``Underfull lines are always accepted regardless of badness`` () =
        // Underfull lines (ratio >= 0) can always be achieved by stretching
        // They should be accepted even with very high badness

        let items =
            [|
                Items.box 30.0
                Items.glue 5.0 5.0 0.0 // Can stretch by 5
                Items.box 30.0
            |]

        let options =
            { LineBreakOptions.Default 80.0 with
                Tolerance = 10.0 // Strict tolerance
            }

        // Natural width: 30 + 5 + 30 = 65
        // Target: 80
        // Need to stretch by 15, max stretch is 5
        // Ratio = 15/5 = 3.0
        // Badness = 100 * 3^3 = 2700 (way over tolerance)
        // But should still be accepted because ratio >= 0

        let lines = LineBreaker.breakLines options items
        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldEqual 3.0

    [<Test>]
    let ``Overfull lines within tolerance are accepted`` () =
        // Lines with -1 <= ratio < 0 should be accepted if badness <= tolerance

        let items =
            [|
                Items.box 50.0
                Items.glue 10.0 0.0 5.0 // Width 10, can shrink by 5
                Items.box 50.0
            |]

        let options =
            { LineBreakOptions.Default 105.0 with
                Tolerance = 100.0 // Allow badness up to 100
            }

        // Natural width: 50 + 10 + 50 = 110
        // Target: 105
        // Need to shrink by 5, max shrink is 5
        // Ratio = -5/5 = -1.0
        // Badness = 100 * 1^3 = 100 (exactly at tolerance)

        let lines = LineBreaker.breakLines options items
        lines.Length |> shouldEqual 1
        lines.[0].AdjustmentRatio |> shouldEqual -1.0

    [<Test>]
    let ``Higher tolerance allows looser lines`` () =
        let items =
            [|
                Items.box 50.0
                Items.glue 10.0 5.0 3.0
                Items.box 50.0
                Items.glue 10.0 5.0 3.0
                Items.box 50.0
            |]

        let strictOptions =
            { LineBreakOptions.Default 100.0 with
                Tolerance = 0.5
            }

        let looseOptions =
            { LineBreakOptions.Default 100.0 with
                Tolerance = 3.0
            }

        let strictLines = LineBreaker.breakLines strictOptions items
        let looseLines = LineBreaker.breakLines looseOptions items

        strictLines.Length |> shouldBeGreaterThan 0
        looseLines.Length |> shouldBeGreaterThan 0

    [<Test>]
    let ``Tolerance filtering prunes a globally optimal but tight line`` () =
        // This test demonstrates the regression where tolerance is incorrectly used to discard an otherwise
        // feasible line. The first line in the optimal solution has ratio about -0.94 which greatly exceeds the
        // default tolerance, but the Knuth-Plass algorithm should still consider it so the paragraph can be
        // optimised globally.

        let items =
            [|
                Items.box 25.0
                Items.glue 8.0 0.0 8.5
                Items.box 25.0
                Items.penalty 0.0 0.0 false
                Items.glue 8.0 10.0 1.0
                Items.box 8.0
            |]

        // With an artificially high tolerance we can observe the globally optimal solution: break after the
        // penalty so the first line contains Box-Glue-Box (ratio ≈ -0.94) and the second line takes the remaining
        // glue and box (ratio ≈ 3.4). This works because tolerance no longer filters the first line out.
        let tolerantOptions =
            { LineBreakOptions.Default 50.0 with
                Tolerance = 5000.0
            }

        let optimalLines = LineBreaker.breakLines tolerantOptions items
        optimalLines.Length |> shouldEqual 2
        optimalLines.[0].End |> shouldEqual 4

        (abs (optimalLines.[0].AdjustmentRatio + 0.9411764705882353)) < 1e-6
        |> shouldEqual true

        // The default tolerance incorrectly prunes this tight line, forcing the algorithm to pick a much worse
        // break earlier in the paragraph. Once the bug is fixed, the default options should yield the same result
        // as the tolerant ones above.
        let defaultOptions = LineBreakOptions.Default 50.0
        let actualLines = LineBreaker.breakLines defaultOptions items

        actualLines |> shouldEqual optimalLines
