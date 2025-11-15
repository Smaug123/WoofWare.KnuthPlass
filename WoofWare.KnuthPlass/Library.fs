namespace WoofWare.KnuthPlass

type Box = private | Box

module Say =
    let hello name = printfn "Hello %s" name
