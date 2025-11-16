namespace WoofWare.KnuthPlass

open System

[<RequireQualifiedAccess>]
module Items =
    /// Creates a box with the given width
    let box (width : float) : Item =
        Box
            {
                Width = width
            }

    /// Creates glue with the given width, stretch, and shrink values
    let glue (width : float) (stretch : float) (shrink : float) : Item =
        Glue
            {
                Width = width
                Stretch = stretch
                Shrink = shrink
            }

    /// Creates a penalty with the given width, cost, and flagged status
    let penalty (width : float) (cost : float) (flagged : bool) : Item =
        Penalty
            {
                Width = width
                Cost = cost
                Flagged = flagged
            }

    /// Creates a forced break (infinite penalty against not breaking)
    let forcedBreak () : Item = penalty 0.0 (-infinity) false

    /// Converts a simple string into a list of items (boxes for words, glue for spaces)
    let fromString (text : string) (wordWidth : string -> float) (spaceWidth : float) : Item list =
        let words =
            text.Replace("\r", "").Split ([| ' ' ; '\n' |], StringSplitOptions.RemoveEmptyEntries)

        [
            for i, word in Array.indexed words do
                yield box (wordWidth word)

                // Add glue between words (but not after the last word)
                if i < words.Length - 1 then
                    yield glue spaceWidth (spaceWidth * 0.5) (spaceWidth * 0.333)
        ]
