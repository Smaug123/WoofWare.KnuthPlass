# WoofWare.KnuthPlass

[![NuGet version](https://img.shields.io/nuget/v/WoofWare.KnuthPlass.svg?style=flat-square)](https://www.nuget.org/packages/WoofWare.KnuthPlass)
[![GitHub Actions status](https://github.com/Smaug123/WoofWare.KnuthPlass/actions/workflows/dotnet.yaml/badge.svg)](https://github.com/Smaug123/WoofWare.KnuthPlass/actions?query=branch%3Amain)
[![License file](https://img.shields.io/github/license/Smaug123/WoofWare.KnuthPlass)](./LICENSE.md)

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="logos/dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="logos/light.svg">
  <img alt="Project logo: minimalistic face of a cartoon Shiba Inu; its right-hand edge is composed of neatly justified horizontal lines." src="logos/light.svg" width="300">
</picture>

The [Knuth-Plass line breaking algorithm](https://en.wikipedia.org/wiki/Knuth%E2%80%93Plass_line-breaking_algorithm).
(See [Gwern's mirror of the paper](https://gwern.net/doc/design/typography/tex/1981-knuth.pdf).)
This is almost entirely coded by Claude Sonnet 4.5 and GPT-5.1-codex, with review from Sonnet 4.5, Gemini 2.5 Pro, and GPT-5.

## How to use

The main function is `Text.formatEnglish` (or the underlying `Text.format`, which is much more flexible).
Simply pass it the desired width of the display portal, and the text you wish to be reflowed to fit in that portal.
We respect any line breaks that exist in the original, but consider ourselves free to break lines at any `' '`
characters too.
We are also free to break lines at hyphenation points (see "Hyphenation" below), but will try not to do that (balancing
with the desire not to have lines of too jagged a length),
and we will try very hard not to break the same word with more than one hyphen.

While `Text.format` provides a string-oriented view of the world, you can use the underlying primitives of `Items` if you like,
to lay out arbitrary things-with-widths ("boxes") in the presence of spaces-which-can-grow-or-shrink ("glue") and with
arbitrary places ("penalties") to insert breaks of arbitrary width.
We don't really give you any help for this beyond `LineBreaker.breakLines`; feel free to request features if you find yourself doing this.

## Status

This appears to work, although my own understanding of Knuth-Plass is pretty shaky.
It's not seen any prod use yet.
Bug reports are welcome.

## Design goals

The reason I wrote this library was for use in TUIs; specifically, this is most of the implementation of a feature for my TUI framework, [WoofWare.Zoomies](https://github.com/Smaug123/WoofWare.Zoomies/issues/78).
As such, it is intended to be performant enough to run pretty regularly, e.g. to dynamically reflow text as a window is resized.
I consider it a performance bug if it is not performant enough to do that.
(As I say, I haven't actually used this in prod yet, so I don't know the answer to that question.)

## Limitations

### Hyphenation
Hyphenation is *barely* implemented, despite being kind of a critical part of a text layout algorithm.
We currently use the most naive possible algorithm, of allowing hyphenation after vowel clusters.
This is obviously terrible, and in the future we should use Knuth-Liang.

You are free to plug in your own hyphenation system by calling e.g. `Text.format` rather than the usual
`Text.formatEnglish` which wraps it.

## Contributions

Feel free to send me tests indicating that the library is laying something out badly; I'll probably fix them, because I find such failures aesthetically annoying.
Similarly, point out any obvious correctness bugs in what's already there, or demonstrate (with numbers!) opportunities to improve performance easily.

I'm pretty unlikely to add whole new features for you, but am probably willing to review changes that add them for me if they come with tests.

## Licence

This project is licensed to you under the MIT licence, a copy of which can be found at [./LICENSE.md](./LICENSE.md).

Texts of the example text files are in the public domain; these are contained in the `WoofWare.KnuthPlass.Test/publicdomain` folder.
