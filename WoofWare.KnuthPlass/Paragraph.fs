namespace WoofWare.KnuthPlass

open System
open System.Globalization

/// A module providing helper functions for producing text specified as .NET strings rather than as detailed layout
/// information.
[<RequireQualifiedAccess>]
module Paragraph =
    let private defaultWordWidth (s : string) = float (StringInfo(s).LengthInTextElements)

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
            // Create a mapping from item index to word index (only for Box items)
            let boxToWord =
                items
                |> List.indexed
                |> List.choose (fun (i, item) ->
                    match item with
                    | Box _ -> Some i
                    | _ -> None
                )
                |> List.indexed
                |> List.map (fun (wordIdx, itemIdx) -> (itemIdx, wordIdx))
                |> Map.ofList

            let lineTexts =
                lines
                |> List.map (fun line ->
                    // Get all box indices in this line and map to words
                    [ line.Start .. line.End - 1 ]
                    |> List.choose (fun i -> Map.tryFind i boxToWord)
                    |> List.map (fun wordIdx -> words.[wordIdx])
                    |> String.concat " "
                )

            String.concat Environment.NewLine lineTexts

    /// Formats text into a paragraph with line breaks using the Knuth-Plass algorithm.
    /// Returns the text with line breaks inserted at 'optimal' positions.
    let format (lineWidth : float) (text : string) : string =
        format' lineWidth defaultWordWidth 1.0 text
