namespace WoofWare.KnuthPlass

open System

/// Provides hyphenation strategies for use with Items.fromString
[<RequireQualifiedAccess>]
module Hyphenation =
    /// Cost penalty to apply to the addition of a single hyphen.
    [<Literal>]
    let DEFAULT_PENALTY = 50.0

    /// Simple English hyphenation following TeX rules:
    /// - Minimum word length: 5 characters
    /// - lefthyphenmin: 2 (at least 2 characters before hyphen)
    /// - righthyphenmin: 3 (at least 3 characters after hyphen)
    /// This is a naive implementation that allows breaks after every vowel cluster.
    /// For production use, consider a dictionary-based approach like the Liang algorithm.
    let simpleEnglish (word : string) : int list =
        if word.Length < 5 then
            []
        else
            // Allow hyphenation between characters, respecting min constraints
            // Prefer breaking after vowels (a simple heuristic)
            let isVowel (c : char) = "aeiouyAEIOUY".Contains c

            [
                for i in 2 .. word.Length - 4 do
                    // Prefer breaking after vowels, before consonants
                    if i > 0 && i < word.Length && isVowel word.[i - 1] && not (isVowel word.[i]) then
                        yield i
            ]

    /// No hyphenation - returns empty list for all words
    let none (_ : string) : int list = []

    /// Allows hyphenation at every valid position (respecting TeX minimums).
    /// This is very aggressive and mainly useful for testing.
    let everywhere (word : string) : int list =
        if word.Length < 5 then [] else [ 2 .. word.Length - 4 ]

/// Module for manipulating Item.
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

    /// Creates word items with optional hyphenation support
    let private createWordItems
        (word : string)
        (wordWidth : string -> float)
        (hyphenWidth : float)
        (hyphenPenalty : float)
        (getHyphenationPoints : string -> int list)
        : Item list
        =
        let points =
            getHyphenationPoints word
            |> List.filter (fun i -> i > 0 && i < word.Length)
            |> List.sort
            |> List.distinct

        if List.isEmpty points then
            [ box (wordWidth word) ]
        else
            // Split word at hyphenation points and insert penalties
            let mutable lastIdx = 0

            [
                for point in points do
                    // Box for the part before this hyphenation point
                    let part = word.Substring (lastIdx, point - lastIdx)
                    yield box (wordWidth part)

                    // Penalty for hyphenation (flagged to prevent consecutive hyphens)
                    yield penalty hyphenWidth hyphenPenalty true
                    lastIdx <- point

                // Final box for the remaining part
                let finalPart = word.Substring lastIdx
                yield box (wordWidth finalPart)
            ]

    /// Converts a simple string into a list of items (boxes for words, glue for spaces, forced breaks for newlines).
    /// Optionally supports hyphenation by providing a function that returns valid hyphenation indices for each word.
    /// Hyphenation follows TeX conventions: penalty items are flagged and have the specified cost.
    let fromString
        (wordWidth : string -> float)
        (spaceWidth : float)
        (getHyphenationPoints : string -> int list)
        (hyphenPenalty : float)
        (text : string)
        : Item list
        =
        let normalized = text.Replace ("\r", "")
        let paragraphs = normalized.Split '\n'
        let hyphenWidth = wordWidth "-"

        [
            for paragraph in paragraphs do
                let words = paragraph.Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

                for j, word in Array.indexed words do
                    // Add boxes (and penalties if hyphenation enabled) for the word
                    yield! createWordItems word wordWidth hyphenWidth hyphenPenalty getHyphenationPoints

                    // Add glue between words (but not after the last word in the paragraph)
                    if j < words.Length - 1 then
                        yield glue spaceWidth (spaceWidth * 0.5) (spaceWidth * 0.333)

                // End each paragraph with infinite-stretch glue and forced break
                yield glue 0.0 infinity 0.0
                yield forcedBreak ()
        ]

    /// Converts a simple string into a list of items (boxes for words, glue for spaces, forced breaks for newlines).
    /// Does not use hyphenation.
    let fromEnglishString (wordWidth : string -> float) (spaceWidth : float) (text : string) : Item list =
        fromString wordWidth spaceWidth Hyphenation.simpleEnglish Hyphenation.DEFAULT_PENALTY text
