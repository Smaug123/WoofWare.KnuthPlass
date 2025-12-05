namespace WoofWare.KnuthPlass

open System

/// Provides hyphenation strategies for use with Items.fromString
[<RequireQualifiedAccess>]
module Hyphenation =
    /// Cost penalty to apply to the addition of a single hyphen.
    [<Literal>]
    let DEFAULT_PENALTY = 50.0f

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
                for i in 2 .. word.Length - 3 do
                    // Prefer breaking after vowels, before consonants
                    if i > 0 && i < word.Length && isVowel word.[i - 1] && not (isVowel word.[i]) then
                        yield i
            ]

    /// No hyphenation - returns empty list for all words
    let none (_ : string) : int list = []

    /// Allows hyphenation at every valid position (respecting TeX minimums).
    /// This is very aggressive and mainly useful for testing.
    let everywhere (word : string) : int list =
        if word.Length < 5 then [] else [ 2 .. word.Length - 3 ]

/// Module for manipulating Item.
[<RequireQualifiedAccess>]
module Items =
    /// Creates a box with the given width
    let box (width : float32) : Item =
        Box
            {
                Width = width
            }

    /// Creates glue with the given width, stretch, and shrink values
    let glue (width : float32) (stretch : float32) (shrink : float32) : Item =
        Glue
            {
                Width = width
                Stretch = stretch
                Shrink = shrink
            }

    /// Creates a penalty with the given width, cost, and flagged status
    let penalty (width : float32) (cost : float32) (flagged : bool) : Item =
        Penalty
            {
                Width = width
                Cost = cost
                Flagged = flagged
            }

    /// Creates a forced break (infinite penalty against not breaking)
    let forcedBreak () : Item =
        penalty 0.0f Single.NegativeInfinity false

    /// Creates word items with optional hyphenation support
    let private createWordItems
        (word : string)
        (wordWidth : string -> float32)
        (hyphenWidth : float32)
        (hyphenPenalty : float32)
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

    /// A glue of the given width, intended for appearing between words.
    ///
    /// A more complex system like TeX treats the glue after punctuation marks differently from the glue between words.
    /// WoofWare.KnuthPlass doesn't do that out of the box - `defaultGlue` is used for both - but you are free to use
    /// the primitives to write this if you like.
    let defaultGlue (width : float32) : Glue =
        {
            Width = width
            Stretch = width * 0.5f
            Shrink = width / 3.0f
        }

    /// A glue which has width 1, can't shrink, and resists stretching.
    let monospaceGlue : Glue =
        {
            Width = 1.0f
            Stretch = 0.1f
            Shrink = 0.0f
        }

    /// Converts a simple string into a list of items (boxes for words, glue for spaces, forced breaks for newlines).
    /// Optionally supports hyphenation by providing a function that returns valid hyphenation indices for each word.
    /// Hyphenation follows TeX conventions: penalty items are flagged and have the specified `hyphenPenalty` cost.
    ///
    /// A more complex system like TeX treats the glue after punctuation marks differently from the glue between words.
    /// This function doesn't do that, although you are free to write one if you like!
    let fromString
        (wordWidth : string -> float32)
        (spaceWidth : Glue)
        (getHyphenationPoints : string -> int list)
        (hyphenPenalty : float32)
        (text : string)
        : Item[]
        =
        let normalized = text.Replace ("\r", "")
        let paragraphs = normalized.Split '\n'
        let hyphenWidth = wordWidth "-"

        let arr = ResizeArray ()

        for paragraph in paragraphs do
            let words = paragraph.Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

            for j, word in Array.indexed words do
                // Add boxes (and penalties if hyphenation enabled) for the word
                arr.AddRange (createWordItems word wordWidth hyphenWidth hyphenPenalty getHyphenationPoints)

                // Add glue between words (but not after the last word in the paragraph)
                if j < words.Length - 1 then
                    arr.Add (Glue spaceWidth)

            // End each paragraph with infinite-stretch glue and forced break
            glue 0.0f Single.PositiveInfinity 0.0f |> arr.Add
            forcedBreak () |> arr.Add

        arr.ToArray ()

    /// Converts a simple string into a list of items (boxes for words, glue for spaces, forced breaks for newlines).
    /// Uses simple English-language hyphenation rules.
    ///
    /// A more complex system like TeX treats the glue after punctuation marks differently from the glue between words.
    /// This function doesn't do that, although you are free to write one if you like!
    let fromEnglishString (wordWidth : string -> float32) (spaceWidth : float32) (text : string) : Item[] =
        fromString wordWidth (defaultGlue spaceWidth) Hyphenation.simpleEnglish Hyphenation.DEFAULT_PENALTY text
