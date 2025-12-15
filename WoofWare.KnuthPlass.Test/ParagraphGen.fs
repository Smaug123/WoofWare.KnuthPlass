namespace WoofWare.KnuthPlass.Test

open FsCheck
open FsCheck.FSharp
open WoofWare.KnuthPlass

type PenaltySpec =
    {
        Width : float32
        Cost : float32
        Flagged : bool
    }

type GlueSpec =
    {
        Width : float32
        Stretch : float32
        Shrink : float32
    }

/// A segment is: Box, then zero or more Penalties, then Glue.
/// This mirrors the structure "word [hyphenation-points] space".
type SegmentSpec =
    {
        BoxWidth : float32
        Penalties : PenaltySpec list
        Glue : GlueSpec
    }

/// A paragraph is a non-empty list of segments.
/// We use a head + tail representation to guarantee non-emptiness by construction.
type ParagraphSpec =
    {
        Head : SegmentSpec
        Tail : SegmentSpec list
    }

    member this.Segments = this.Head :: this.Tail

/// High-level specification for a paragraph of items.
/// This is what we actually generate; it compiles down to Item[].
/// The structure guarantees validity by construction: every segment has a box,
/// and we always append proper termination.
[<RequireQualifiedAccess>]
module ParagraphSpec =
    /// Compile the high-level spec to an Item array.
    let compile (spec : ParagraphSpec) : Item[] =
        [|
            for seg in spec.Segments do
                yield
                    Item.Box
                        {
                            Width = seg.BoxWidth
                        }

                for pen in seg.Penalties do
                    yield
                        Item.Penalty
                            {
                                Width = pen.Width
                                Cost = pen.Cost
                                Flagged = pen.Flagged
                            }

                yield
                    Item.Glue
                        {
                            Width = seg.Glue.Width
                            Stretch = seg.Glue.Stretch
                            Shrink = seg.Glue.Shrink
                        }

            // Termination: finishing glue (infinite stretch) + forced break
            yield
                Item.Glue
                    {
                        Width = 0.0f
                        Stretch = infinityf
                        Shrink = 0.0f
                    }
            yield
                Item.Penalty
                    {
                        Width = 0.0f
                        Cost = -infinityf
                        Flagged = false
                    }
        |]

/// Generators for ParagraphSpec, parameterized by penalty probability.
[<RequireQualifiedAccess>]
module ParagraphGen =
    let genPenaltySpec : Gen<PenaltySpec> =
        gen {
            // Width: typically small (hyphen width), but allow 0
            let! width = Gen.choose (0, 5) |> Gen.map float32
            // Cost: can be negative (encourage break) or positive (discourage)
            // Avoid infinity here; that's for forced/forbidden breaks
            let! cost = Gen.choose (-100, 100) |> Gen.map float32
            let! flagged = Gen.elements [ true ; false ]

            return
                {
                    Width = width
                    Cost = cost
                    Flagged = flagged
                }
        }

    let genGlueSpec : Gen<GlueSpec> =
        gen {
            // Natural width: positive, representing space between words
            let! width = Gen.choose (1, 10) |> Gen.map float32
            // Stretch: how much it can grow (0 = rigid)
            let! stretch = Gen.choose (0, 5) |> Gen.map float32
            // Shrink: how much it can compress (0 = rigid)
            let! shrink = Gen.choose (0, 3) |> Gen.map float32

            return
                {
                    Width = width
                    Stretch = stretch
                    Shrink = shrink
                }
        }

    /// Generate a segment with penalties appearing with the given probability.
    /// Uses geometric distribution with a cap to prevent unbounded generation.
    let genSegmentSpec (penaltyProbability : float) : Gen<SegmentSpec> =
        let maxPenalties = 10 // Cap to prevent runaway generation

        let rec genPenalties (acc : PenaltySpec list) : Gen<PenaltySpec list> =
            gen {
                if List.length acc >= maxPenalties then
                    return List.rev acc
                else
                    let! roll = Gen.choose (0, 99)
                    let addMore = float roll < penaltyProbability * 100.0

                    if addMore then
                        let! pen = genPenaltySpec
                        return! genPenalties (pen :: acc)
                    else
                        return List.rev acc
            }

        gen {
            // Box width: positive, representing word/content width
            let! boxWidth = Gen.choose (1, 20) |> Gen.map float32
            let! penalties = genPenalties []
            let! glue = genGlueSpec

            return
                {
                    BoxWidth = boxWidth
                    Penalties = penalties
                    Glue = glue
                }
        }

    /// Generate a paragraph spec with the given penalty probability.
    let genParagraphSpec (penaltyProbability : float) : Gen<ParagraphSpec> =
        gen {
            let! segmentCount = Gen.choose (0, 19) // 0-19 tail segments = 1-20 total
            let! head = genSegmentSpec penaltyProbability
            let! tail = Gen.listOfLength segmentCount (genSegmentSpec penaltyProbability)

            return
                {
                    Head = head
                    Tail = tail
                }
        }

    /// Generate a (penaltyProbability, paragraphSpec, lineWidth) triple.
    /// Line width is correlated with content to ensure interesting test scenarios:
    /// - Low multipliers (30-70%) produce tight/overfull lines
    /// - High multipliers (100-150%) produce loose/underfull lines
    let genTestCase : Gen<float * ParagraphSpec * float32> =
        gen {
            let! penaltyProb = Gen.choose (0, 100) |> Gen.map (fun x -> float x / 100.0)
            let! spec = genParagraphSpec penaltyProb

            // Compute content metrics
            let totalBoxWidth = spec.Segments |> List.sumBy (fun s -> s.BoxWidth)
            let avgSegmentWidth = totalBoxWidth / float32 (List.length spec.Segments)

            // Generate line width as 30%-150% of average segment width
            // This ensures meaningful relationship between content and line width
            let! multiplier = Gen.choose (30, 150) |> Gen.map (fun x -> float32 x / 100.0f)
            let lineWidth = max 1.0f (avgSegmentWidth * multiplier)

            return (penaltyProb, spec, lineWidth)
        }
