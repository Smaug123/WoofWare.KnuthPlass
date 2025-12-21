namespace WoofWare.KnuthPlass

open System
open System.Collections.Generic
open System.Globalization
open System.Text

/// A module providing helper functions for producing text specified as .NET strings rather than as detailed layout
/// information.
///
/// These functions take callback parameters for measuring and hyphenation, so they are "shell" level conveniences
/// rather than pure primitives. For maximum control, use Items and LineBreaker directly.
[<RequireQualifiedAccess>]
module Text =
    /// Compute the display width of a string, in a fixed-width font.
    let defaultWordWidth (s : string) : float32 =
        float32 (StringInfo(s).LengthInTextElements)

    /// Width of a single space character.
    [<Literal>]
    let SPACE_WIDTH = 1.0f

    /// Split a word at the given hyphenation points (positions).
    let private splitAtPoints (word : string) (points : struct (int * float32) array) : string array =
        if points.Length = 0 then
            [| word |]
        else
            // Filter to valid range, sort, dedupe
            let validPoints =
                points
                |> Array.filter (fun struct (pos, _) -> pos > 0 && pos < word.Length)
                |> Array.sortBy (fun struct (pos, _) -> pos)
                |> Array.distinctBy (fun struct (pos, _) -> pos)

            if validPoints.Length = 0 then
                [| word |]
            else
                let result = ResizeArray ()
                let mutable lastIdx = 0

                for struct (pos, _) in validPoints do
                    result.Add (word.Substring (lastIdx, pos - lastIdx))
                    lastIdx <- pos

                result.Add (word.Substring lastIdx)
                result.ToArray ()

    /// Split a word into fragments based on hyphenation, returning both fragments and their penalty values.
    let private computeWordParts
        (basePenalty : float32)
        (hyphenate : string -> byte array)
        (word : string)
        : struct (string array * float32 array)
        =
        let priorities = hyphenate word
        let points = Hyphenation.prioritiesToPoints basePenalty priorities
        let fragments = splitAtPoints word points

        let penalties =
            if fragments.Length = 1 then
                [||]
            else
                points
                |> Array.filter (fun struct (pos, _) -> pos > 0 && pos < word.Length)
                |> Array.sortBy (fun struct (pos, _) -> pos)
                |> Array.distinctBy (fun struct (pos, _) -> pos)
                |> Array.map (fun struct (_, pen) -> pen)

        struct (fragments, penalties)

    /// Create items for a single word given pre-computed fragments and penalties.
    let private wordPartsToItems
        (wordWidth : string -> float32)
        (hyphenWidth : float32)
        (fragments : string array)
        (penalties : float32 array)
        : Item[]
        =
        if fragments.Length = 1 then
            [| Items.box (wordWidth fragments.[0]) |]
        else
            let fragmentWidths = fragments |> Array.map wordWidth
            Items.wordFromFragments hyphenWidth (ReadOnlySpan fragmentWidths) (ReadOnlySpan penalties)

    /// Create items for a paragraph (no newlines), also returning the word parts for text reconstruction.
    let private paragraphToItemsWithParts
        (wordWidth : string -> float32)
        (spaceWidth : Glue)
        (hyphenWidth : float32)
        (basePenalty : float32)
        (hyphenate : string -> byte array)
        (words : string array)
        : struct (Item[] * string array array)
        =
        if words.Length = 0 then
            struct ([| Items.glue 0.0f Single.PositiveInfinity 0.0f ; Items.forcedBreak () |], [||])
        else
            let result = ResizeArray ()
            let wordParts = Array.zeroCreate words.Length

            for i, word in Array.indexed words do
                let struct (fragments, penalties) = computeWordParts basePenalty hyphenate word
                wordParts.[i] <- fragments
                result.AddRange (wordPartsToItems wordWidth hyphenWidth fragments penalties)

                if i < words.Length - 1 then
                    result.Add (Glue spaceWidth)

            result.Add (Items.glue 0.0f Single.PositiveInfinity 0.0f)
            result.Add (Items.forcedBreak ())
            struct (result.ToArray (), wordParts)

    /// Formats a single paragraph (no newlines) into lines using the Knuth-Plass algorithm.
    /// Returns the paragraph text with line breaks inserted at 'optimal' positions.
    let private formatParagraph
        (options : LineBreakOptions)
        (wordWidth : string -> float32)
        (spaceWidth : Glue)
        (basePenalty : float32)
        (hyphenate : string -> byte array)
        (paragraph : string)
        : string
        =
        let words = paragraph.Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

        if words.Length = 0 then
            ""
        else
            let struct (items, wordParts) =
                paragraphToItemsWithParts wordWidth spaceWidth (wordWidth "-") basePenalty hyphenate words

            let lines = LineBreaker.breakLines options items

            // Create mapping from box index to word part text
            let boxToText = Dictionary<int, string> ()
            let mutable boxIdx = 0

            for wordIdx in 0 .. words.Length - 1 do
                for part in wordParts.[wordIdx] do
                    boxToText.[boxIdx] <- part
                    boxIdx <- boxIdx + 1

            let boxToText = boxToText :> IReadOnlyDictionary<int, string>

            // Precompute mapping from item index to box number
            let itemIndexToBoxNumber = Dictionary<int, int> ()
            let mutable currentBoxNum = 0

            for i in 0 .. items.Length - 1 do
                match items.[i] with
                | Box _ ->
                    itemIndexToBoxNumber.[i] <- currentBoxNum
                    currentBoxNum <- currentBoxNum + 1
                | _ -> ()

            let itemIndexToBoxNumber = itemIndexToBoxNumber :> IReadOnlyDictionary<int, int>

            // Reconstruct text for each line
            let result = StringBuilder ()
            let hasBoxAtOrAfter = Dictionary<int, bool> ()

            for line in lines do
                hasBoxAtOrAfter.Clear ()
                let mutable foundBox = false

                for i in line.End - 1 .. -1 .. line.Start do
                    match items.[i] with
                    | Box _ -> foundBox <- true
                    | _ -> ()

                    hasBoxAtOrAfter.[i] <- foundBox

                let hasBoxAtOrAfter = hasBoxAtOrAfter :> IReadOnlyDictionary<int, bool>

                let mutable lastWasBox = false

                for i in line.Start .. line.End - 1 do
                    match items.[i] with
                    | Box _ ->
                        match itemIndexToBoxNumber.TryGetValue i with
                        | true, boxNum ->
                            match boxToText.TryGetValue boxNum with
                            | true, textpart -> result.Append textpart |> ignore<StringBuilder>
                            | false, _ -> ()
                        | false, _ -> ()

                        lastWasBox <- true

                    | Glue _ when lastWasBox && i < line.End - 1 ->
                        match hasBoxAtOrAfter.TryGetValue (i + 1) with
                        | true, hasBox when hasBox -> result.Append ' ' |> ignore<StringBuilder>
                        | _ -> ()

                    | Penalty pen when i = line.End - 1 && pen.Width > 0.0f ->
                        result.Append '-' |> ignore<StringBuilder>

                    | _ -> ()

                result.Append Environment.NewLine |> ignore<StringBuilder>

            if result.Length >= Environment.NewLine.Length then
                result.Remove (result.Length - Environment.NewLine.Length, Environment.NewLine.Length)
                |> ignore<StringBuilder>

            (result : StringBuilder).ToString ()

    /// Formats text into paragraphs with line breaks using the Knuth-Plass algorithm.
    /// Returns the text with line breaks inserted at 'optimal' positions.
    /// Newlines in the input are preserved as paragraph breaks (hard breaks).
    ///
    /// By specifying `wordWidth`, `spaceWidth`, and `basePenalty` appropriately, you can get the string in a form
    /// that would render in any font.
    ///
    /// The `hyphenate` function should return Liang-style priorities: a byte array with one element per inter-letter
    /// position, where odd values indicate valid hyphenation points (all odd values are treated equally).
    ///
    /// Note that the resulting text may contain "overfull" lines: lines which don't fit into the line width.
    /// It's up to you to deal with this appropriately.
    let format
        (options : LineBreakOptions)
        (wordWidth : string -> float32)
        (spaceWidth : Glue)
        (basePenalty : float32)
        (hyphenate : string -> byte array)
        (text : string)
        : string
        =
        let normalizedText = text.Replace ("\r", "")

        normalizedText.Split '\n'
        |> Seq.map (fun para -> formatParagraph options wordWidth spaceWidth basePenalty hyphenate para)
        |> String.concat Environment.NewLine
