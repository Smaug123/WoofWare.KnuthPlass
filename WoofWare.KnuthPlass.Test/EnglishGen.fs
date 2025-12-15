namespace WoofWare.KnuthPlass.Test

open FsCheck
open FsCheck.FSharp

[<RequireQualifiedAccess>]
module EnglishGen =
    /// Generates a non-empty string of lowercase letters and spaces
    let text : Gen<string> =
        Gen.choose (1, 20)
        |> Gen.bind (fun wordCount ->
            let genWord =
                Gen.choose (1, 10)
                |> Gen.bind (fun len ->
                    Gen.elements [ 'a' .. 'z' ]
                    |> Gen.listOfLength len
                    |> Gen.map (fun chars -> System.String (chars |> List.toArray))
                )

            Gen.listOfLength wordCount genWord |> Gen.map (String.concat " ")
        )
