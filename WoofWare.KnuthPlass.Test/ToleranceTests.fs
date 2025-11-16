namespace WoofWare.KnuthPlass.Test

open NUnit.Framework
open WoofWare.KnuthPlass
open FsUnitTyped

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
    let ``Tolerance does not reject feasible lines in global optimization`` () =
        // This test demonstrates that tolerance should NOT be used to reject feasible lines.
        // The optimal solution may require accepting a tight line (high badness) to avoid worse alternatives.

        let items =
            [|
                Items.box 25.0
                Items.glue 8.0 0.0 10.0 // Can shrink by 10
                Items.box 25.0
                Items.glue 8.0 10.0 1.0 // Can stretch by 10
                Items.box 8.0
            |]

        let options = LineBreakOptions.Default 50.0
        // Default tolerance is 10.0

        // Optimal solution: Break after second box (index 3)
        // Line 1: Box(25) Glue(8,0,10) Box(25) = natural width 58
        //   Needs to fit in 50, so diff = -8, shrink = 10
        //   Ratio = -8/10 = -0.8
        //   Badness = 100 * 0.8^3 = 51.2 > 10.0 (exceeds default tolerance)
        // Line 2: Glue(8,10,1) Box(8) = natural width 16
        //   Needs to fit in 50, so diff = 34, stretch = 10
        //   Ratio = 34/10 = 3.4

        // With the bug (badness filtering), line 1 is rejected, forcing all content on one line:
        // All items: natural width 74, needs to fit in 50, diff = -24, shrink = 11
        // Ratio = -24/11 ≈ -2.18 < -1.0 (IMPOSSIBLE - would throw exception)

        // Without the bug, the algorithm accepts the tight line as part of the global optimum
        let lines = LineBreaker.breakLines options items
        lines.Length |> shouldEqual 2
        lines.[0].End |> shouldEqual 3
        lines.[0].AdjustmentRatio |> shouldEqual -0.8
