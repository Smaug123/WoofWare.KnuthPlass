namespace WoofWare.KnuthPlass

open System
open System.Collections.Generic
open System.Globalization
open System.Text

/// A module providing helper functions for producing text specified as .NET strings rather than as detailed layout
/// information.
[<RequireQualifiedAccess>]
module Text =
    /// Compute the display width of a string.
    let defaultWordWidth (s : string) : float =
        float (StringInfo(s).LengthInTextElements)

    /// Width of a single space character.
    [<Literal>]
    let SPACE_WIDTH = 1.0

    /// Formats text into paragraphs with line breaks using the Knuth-Plass algorithm.
    /// Returns the text with line breaks inserted at 'optimal' positions.
    /// Newlines in the input are preserved as paragraph breaks (hard breaks).
    ///
    /// This method can throw! If your constraints are impossible to satisfy (e.g. a word admits no hyphenation,
    /// and is longer than the available width), we throw.
    let format
        (lineWidth : float)
        (wordWidth : string -> float)
        (spaceWidth : float)
        (hyphenPenalty : float)
        (getHyphenationPoints : string -> int list)
        (text : string)
        : string
        =
        // Normalize and split into paragraphs, then words, to match Items.fromString behavior
        let normalizedText = text.Replace ("\r", "")
        let paragraphs = normalizedText.Split '\n'

        let words =
            paragraphs
            |> Array.collect (fun p -> p.Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries))

        if words.Length = 0 then
            ""
        else
            let items =
                Items.fromString wordWidth spaceWidth getHyphenationPoints hyphenPenalty text

            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items

            // Pre-compute word parts based on hyphenation points
            let wordParts =
                words
                |> Array.map (fun word ->
                    let points =
                        getHyphenationPoints word
                        |> List.filter (fun i -> i > 0 && i < word.Length)
                        |> List.sort
                        |> List.distinct

                    if List.isEmpty points then
                        [| word |]
                    else
                        let parts = ResizeArray ()
                        let mutable lastIdx = 0

                        for point in points do
                            parts.Add (word.Substring (lastIdx, point - lastIdx))
                            lastIdx <- point

                        parts.Add (word.Substring lastIdx)
                        parts.ToArray ()
                )

            // Create mapping from box index to word part text
            let boxToText = Dictionary<int, string> ()
            let mutable boxIdx = 0

            for wordIdx in 0 .. words.Length - 1 do
                for part in wordParts.[wordIdx] do
                    boxToText.[boxIdx] <- part
                    boxIdx <- boxIdx + 1

            let boxToText = boxToText :> IReadOnlyDictionary<int, string>

            // Precompute mapping from item index to box number (O(n) instead of O(n²))
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
                // Precompute whether there's a box at or after each position (O(n) instead of O(n²))
                hasBoxAtOrAfter.Clear ()
                let mutable foundBox = false

                for i in line.End - 1 .. -1 .. line.Start do
                    match items.[i] with
                    | Box _ -> foundBox <- true
                    | _ -> ()

                    hasBoxAtOrAfter.[i] <- foundBox

                let hasBoxAtOrAfter = hasBoxAtOrAfter :> IReadOnlyDictionary<int, bool>

                // Join parts and add spaces between words
                // Need to track where glue was to add spaces
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
                        // Add space if there's content after this glue in the line
                        match hasBoxAtOrAfter.TryGetValue (i + 1) with
                        | true, hasBox when hasBox -> result.Append ' ' |> ignore<StringBuilder>
                        | _ -> ()

                    | Penalty pen when i = line.End - 1 && pen.Width > 0.0 -> result.Append '-' |> ignore<StringBuilder>

                    | _ -> ()

                result.Append Environment.NewLine |> ignore<StringBuilder>

            // we now have a rogue final newline from the last loop!
            result.Remove (result.Length - Environment.NewLine.Length, Environment.NewLine.Length)
            |> ignore<StringBuilder>

            (result : StringBuilder).ToString ()

    /// Formats text into paragraphs with line breaks using the Knuth-Plass algorithm.
    /// Returns the text with line breaks inserted at 'optimal' positions.
    /// Newlines in the input are preserved as paragraph breaks (hard breaks).
    ///
    /// This method can throw! If your constraints are impossible to satisfy (e.g. a word admits no hyphenation,
    /// and is longer than the available width), we throw.
    let formatEnglish (lineWidth : float) (text : string) : string =
        format lineWidth defaultWordWidth SPACE_WIDTH Hyphenation.DEFAULT_PENALTY Hyphenation.simpleEnglish text
