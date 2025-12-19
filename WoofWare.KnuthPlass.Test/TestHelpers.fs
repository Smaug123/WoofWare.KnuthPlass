namespace WoofWare.KnuthPlass.Test

open System
open System.Globalization
open WoofWare.KnuthPlass

/// Test helpers for creating items conveniently.
/// These are NOT part of the public API - they exist only for testing.
[<RequireQualifiedAccess>]
module TestHelpers =
    /// No hyphenation - returns empty priorities for all words.
    let noHyphenation (_ : string) : byte array = [||]

    /// Simple hyphenation that allows breaks at every valid position (for testing).
    /// Returns priorities array where positions 2..len-3 have priority 1.
    let everywhereHyphenation (word : string) : byte array =
        if word.Length < 5 then
            [||]
        else
            // Create priorities for inter-letter positions (length - 1 positions)
            let priorities = Array.zeroCreate (word.Length - 1)
            // Set odd priorities (valid hyphenation) for positions 2..len-3
            // Position i in the priorities array corresponds to breaking after character i
            for i in 1 .. word.Length - 4 do
                priorities.[i] <- 1uy

            priorities

    /// Compute the display width of a string, in a fixed-width font.
    let defaultWordWidth (s : string) : float32 =
        float32 (StringInfo(s).LengthInTextElements)

    /// Create items from text with no hyphenation (for simple tests).
    let fromStringNoHyphenation (text : string) : Item array =
        let spaceGlue = Items.defaultGlue 1.0f
        let normalized = text.Replace ("\r", "")
        let paragraphs = normalized.Split '\n'
        let result = ResizeArray ()

        for paragraph in paragraphs do
            let words = paragraph.Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

            for i, word in Array.indexed words do
                result.Add (Items.box (defaultWordWidth word))

                if i < words.Length - 1 then
                    result.Add (Glue spaceGlue)

            result.Add (Items.glue 0.0f Single.PositiveInfinity 0.0f)
            result.Add (Items.forcedBreak ())

        result.ToArray ()

    /// Create items from text with simple hyphenation (for tests that need hyphenation).
    let fromStringWithHyphenation (hyphenate : string -> byte array) (text : string) : Item array =
        let spaceGlue = Items.defaultGlue 1.0f
        let hyphenWidth = 1.0f
        let basePenalty = Hyphenation.DEFAULT_PENALTY
        let normalized = text.Replace ("\r", "")
        let paragraphs = normalized.Split '\n'
        let result = ResizeArray ()

        for paragraph in paragraphs do
            let words = paragraph.Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

            for i, word in Array.indexed words do
                let priorities = hyphenate word
                let points = Hyphenation.prioritiesToPoints basePenalty priorities

                if points.Length = 0 then
                    result.Add (Items.box (defaultWordWidth word))
                else
                    // Split word at hyphenation points
                    let validPoints =
                        points
                        |> Array.filter (fun struct (pos, _) -> pos > 0 && pos < word.Length)
                        |> Array.sortBy (fun struct (pos, _) -> pos)
                        |> Array.distinctBy (fun struct (pos, _) -> pos)

                    if validPoints.Length = 0 then
                        result.Add (Items.box (defaultWordWidth word))
                    else
                        let fragments = ResizeArray ()
                        let mutable lastIdx = 0

                        for struct (pos, _) in validPoints do
                            fragments.Add (word.Substring (lastIdx, pos - lastIdx))
                            lastIdx <- pos

                        fragments.Add (word.Substring lastIdx)

                        let fragmentWidths = fragments.ToArray () |> Array.map defaultWordWidth
                        let penalties = validPoints |> Array.map (fun struct (_, pen) -> pen)

                        result.AddRange (
                            Items.wordFromFragments hyphenWidth (ReadOnlySpan fragmentWidths) (ReadOnlySpan penalties)
                        )

                if i < words.Length - 1 then
                    result.Add (Glue spaceGlue)

            result.Add (Items.glue 0.0f Single.PositiveInfinity 0.0f)
            result.Add (Items.forcedBreak ())

        result.ToArray ()
