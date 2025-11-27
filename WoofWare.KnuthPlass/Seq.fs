namespace WoofWare.KnuthPlass

[<RequireQualifiedAccess>]
module internal Seq =

    // Bafflingly, this isn't in the standard library.
    let tryMinBy (f : 'a -> 'b) (s : 'a seq) : 'a option =
        use e = s.GetEnumerator ()

        if not (e.MoveNext ()) then
            None
        else
            let mutable result = e.Current
            let mutable currentCost = f e.Current

            while e.MoveNext () do
                let newCost = f e.Current

                if newCost < currentCost then
                    currentCost <- newCost
                    result <- e.Current

            Some result
