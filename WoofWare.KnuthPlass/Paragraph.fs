namespace WoofWare.KnuthPlass

open System

[<RequireQualifiedAccess>]
module Paragraph =
    let private defaultWordWidth (s : string) = float s.Length

    /// Formats text into a paragraph with line breaks using the Knuth-Plass algorithm.
    /// Returns the text with line breaks inserted at 'optimal' positions.
    let format' (lineWidth : float) (wordWidth : string -> float) (spaceWidth : float) (text : string) : string =
        let words = text.Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

        if words.Length = 0 then
            ""
        else
            let items = Items.fromString text wordWidth spaceWidth
            let options = LineBreakOptions.Default lineWidth
            let lines = LineBreaker.breakLines options items

            // Extract words for each line
            // Items alternate: box (even indices) and glue (odd indices)
            // Word i is at item index i*2
            let lineTexts =
                lines
                |> List.map (fun line ->
                    // Get all box indices in this line
                    [ line.Start .. line.End - 1 ]
                    |> List.filter (fun i -> i % 2 = 0) // Boxes are at even indices
                    |> List.map (fun i -> words.[i / 2])
                    |> String.concat " "
                )

            String.concat Environment.NewLine lineTexts

    /// Formats text into a paragraph with line breaks using the Knuth-Plass algorithm.
    /// Returns the text with line breaks inserted at 'optimal' positions.
    let format (lineWidth : float) (text : string) : string =
        format' lineWidth defaultWordWidth 1.0 text
