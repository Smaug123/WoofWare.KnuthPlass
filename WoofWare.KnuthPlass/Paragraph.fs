namespace WoofWare.KnuthPlass

open System
open System.Globalization

/// A module providing helper functions for producing text specified as .NET strings rather than as detailed layout
/// information.
[<RequireQualifiedAccess>]
module Paragraph =
    /// Compute the display width of a string.
    let defaultWordWidth (s : string) : float =
        float (StringInfo(s).LengthInTextElements)

    /// Width of a single space character.
    [<Literal>]
    let SPACE_WIDTH = 1.0

    /// Formats text into a paragraph with line breaks using the Knuth-Plass algorithm.
    /// Returns the text with line breaks inserted at 'optimal' positions.
    let format
        (lineWidth : float)
        (wordWidth : string -> float)
        (spaceWidth : float)
        (hyphenPenalty : float)
        (getHyphenationPoints : string -> int list)
        (text : string)
        : string
        =
        let words = text.Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

        if words.Length = 0 then
            ""
        else
            let items =
                Items.fromString wordWidth spaceWidth getHyphenationPoints hyphenPenalty text

            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items
            let itemsArray = items |> List.toArray

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

                        parts.Add (word.Substring (lastIdx))
                        parts.ToArray ()
                )

            // Create mapping from box index to word part text
            let mutable boxToText = Map.empty
            let mutable boxIdx = 0

            for wordIdx in 0 .. words.Length - 1 do
                for part in wordParts.[wordIdx] do
                    boxToText <- Map.add boxIdx part boxToText
                    boxIdx <- boxIdx + 1

            // Reconstruct text for each line
            let lineTexts =
                lines
                |> List.map (fun line ->
                    let result = ResizeArray ()

                    for i in line.Start .. line.End - 1 do
                        match itemsArray.[i] with
                        | Box _ ->
                            // Find which box number this is (counting only boxes)
                            let boxNum =
                                [ 0 .. i - 1 ]
                                |> List.filter (fun j ->
                                    match itemsArray.[j] with
                                    | Box _ -> true
                                    | _ -> false
                                )
                                |> List.length

                            match Map.tryFind boxNum boxToText with
                            | Some text -> result.Add (text)
                            | None -> ()

                        | Glue _ ->
                            // Add space (but not at start/end of line, handled by checking next item)
                            ()

                        | Penalty pen ->
                            // If this is the last item in the line and it has width (hyphen), add it
                            if i = line.End - 1 && pen.Width > 0.0 then
                                result.Add ("-")

                    // Join parts and add spaces between words
                    // Need to track where glue was to add spaces
                    let mutable finalResult = []
                    let mutable lastWasBox = false

                    for i in line.Start .. line.End - 1 do
                        match itemsArray.[i] with
                        | Box _ ->
                            let boxNum =
                                [ 0 .. i - 1 ]
                                |> List.filter (fun j ->
                                    match itemsArray.[j] with
                                    | Box _ -> true
                                    | _ -> false
                                )
                                |> List.length

                            match Map.tryFind boxNum boxToText with
                            | Some textpart -> finalResult <- textpart :: finalResult
                            | None -> ()

                            lastWasBox <- true

                        | Glue _ when lastWasBox && i < line.End - 1 ->
                            // Add space if there's content after this glue in the line
                            let hasBoxAfter =
                                [ i + 1 .. line.End - 1 ]
                                |> List.exists (fun j ->
                                    match itemsArray.[j] with
                                    | Box _ -> true
                                    | _ -> false
                                )

                            if hasBoxAfter then
                                finalResult <- " " :: finalResult

                        | Penalty pen when i = line.End - 1 && pen.Width > 0.0 -> finalResult <- "-" :: finalResult

                        | _ -> ()

                    finalResult |> List.rev |> String.concat ""
                )

            String.concat Environment.NewLine lineTexts

    /// Formats text into a paragraph with line breaks using the Knuth-Plass algorithm.
    /// Returns the text with line breaks inserted at 'optimal' positions.
    let formatEnglish (lineWidth : float) (text : string) : string =
        format lineWidth defaultWordWidth SPACE_WIDTH Hyphenation.DEFAULT_PENALTY Hyphenation.simpleEnglish text
