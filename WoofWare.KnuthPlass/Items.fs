namespace WoofWare.KnuthPlass

open System

/// Utilities for working with hyphenation data.
[<RequireQualifiedAccess>]
module Hyphenation =
    /// Default cost penalty to apply to the addition of a single hyphen.
    [<Literal>]
    let DEFAULT_PENALTY = 50.0f

    /// <summary>
    /// Convert a priorities array (from Liang-style hyphenation) to (position, penalty) array.
    /// </summary>
    /// <remarks>
    /// Liang hyphenation returns priorities at each inter-letter position where odd values
    /// indicate valid hyphenation points. Lower odd numbers are better break points.
    /// This function converts to (position, penalty) pairs where lower priorities get lower penalties.
    /// </remarks>
    /// <param name="basePenalty">Base penalty multiplier. Final penalty = basePenalty * priority.</param>
    /// <param name="priorities">Priority array from Liang hyphenation (one per inter-letter position).</param>
    let prioritiesToPoints (basePenalty : float32) (priorities : byte array) : struct (int * float32) array =
        let result = ResizeArray ()

        for i = 0 to priorities.Length - 1 do
            let p = priorities.[i]

            if p % 2uy = 1uy then
                // Odd priority = valid hyphenation point
                // Position is i+1 (index into word, after the i-th character)
                result.Add (struct (i + 1, basePenalty * float32 p))

        result.ToArray ()

/// Module for constructing Item values.
[<RequireQualifiedAccess>]
module Items =
    /// Creates a box with the given width.
    let box (width : float32) : Item =
        Box
            {
                Width = width
            }

    /// Creates glue with the given width, stretch, and shrink values.
    let glue (width : float32) (stretch : float32) (shrink : float32) : Item =
        Glue
            {
                Width = width
                Stretch = stretch
                Shrink = shrink
            }

    /// Creates a penalty with the given width, cost, and flagged status.
    let penalty (width : float32) (cost : float32) (flagged : bool) : Item =
        Penalty
            {
                Width = width
                Cost = cost
                Flagged = flagged
            }

    /// Creates a forced break (infinite penalty against not breaking).
    let forcedBreak () : Item =
        penalty 0.0f Single.NegativeInfinity false

    /// <summary>
    /// Converts pre-measured word fragments into an array of items.
    /// </summary>
    /// <remarks>
    /// This is the primitive for hyphenation support. You split the word at hyphenation points
    /// and measure each fragment yourself, then pass the widths and penalties here.
    ///
    /// For a word with N fragments, there are N-1 hyphenation points between them.
    /// For example, "beau-ti-ful" has fragments ["beau", "ti", "ful"] with widths [w1, w2, w3]
    /// and penalties [p1, p2] between them.
    ///
    /// The output is: Box(w1), Penalty(hyphenWidth, p1, flagged=true), Box(w2), Penalty(...), Box(w3)
    /// </remarks>
    /// <param name="hyphenWidth">Width of the hyphen character to insert at breaks.</param>
    /// <param name="fragmentWidths">Width of each word fragment (length N).</param>
    /// <param name="penaltiesBetween">Penalty cost between each pair of fragments (length N-1).</param>
    let wordFromFragments
        (hyphenWidth : float32)
        (fragmentWidths : ReadOnlySpan<float32>)
        (penaltiesBetween : ReadOnlySpan<float32>)
        : Item[]
        =
        if fragmentWidths.Length = 0 then
            [||]
        elif fragmentWidths.Length = 1 then
            [| box fragmentWidths.[0] |]
        else
            // Validate: we need exactly (fragments - 1) penalties
            if penaltiesBetween.Length <> fragmentWidths.Length - 1 then
                invalidArg
                    "penaltiesBetween"
                    $"Expected %d{fragmentWidths.Length - 1} penalties for %d{fragmentWidths.Length} fragments, got %d{penaltiesBetween.Length}"

            let result = Array.zeroCreate (fragmentWidths.Length + penaltiesBetween.Length)
            let mutable idx = 0

            for i = 0 to fragmentWidths.Length - 2 do
                result.[idx] <- box fragmentWidths.[i]
                idx <- idx + 1
                result.[idx] <- penalty hyphenWidth penaltiesBetween.[i] true
                idx <- idx + 1

            // Final fragment (no penalty after)
            result.[idx] <- box fragmentWidths.[fragmentWidths.Length - 1]
            result

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

    /// A glue which has width 1 and can't shrink.
    let monospaceGlue : Glue =
        // The stretch value (0.5) is used internally by the algorithm for optimization
        // but doesn't affect actual rendering - output text still has exactly 1 character
        // per space. Using a higher stretch value allows the algorithm to explore the
        // solution space more effectively, avoiding pathological cases where very short
        // lines are produced.
        {
            Width = 1.0f
            Stretch = 0.5f
            Shrink = 0.0f
        }
