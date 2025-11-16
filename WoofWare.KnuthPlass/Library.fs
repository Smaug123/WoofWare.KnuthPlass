namespace WoofWare.KnuthPlass

/// Represents a fixed-width item (word, character, etc.)
type Box =
    {
        Width : float
    }

/// Represents stretchable/shrinkable whitespace
type Glue =
    {
        Width : float
        Stretch : float
        Shrink : float
    }

/// Represents a potential break point with associated cost
type Penalty =
    {
        Width : float
        Cost : float
        /// True for flagged penalties (e.g., hyphenated breaks)
        Flagged : bool
    }

/// An item in the paragraph to be broken into lines
type Item =
    | Box of Box
    | Glue of Glue
    | Penalty of Penalty

/// Represents a line in the output
type Line =
    {
        /// Index of first item in this line
        Start : int
        /// Index of last item before the break (exclusive)
        End : int
        /// Adjustment ratio: how much glue was stretched (positive) or compressed (negative)
        AdjustmentRatio : float
    }

/// Options for line breaking
type LineBreakOptions =
    {
        /// Target width for each line
        LineWidth : float
        /// Tolerance for acceptable lines (typically 1-3). Higher values allow looser lines.
        Tolerance : float
        /// Penalty for consecutive lines of very different tightness
        AdjacentLooseTightDemerits : float
        /// Penalty for consecutive hyphenated lines
        DoubleHyphenDemerits : float
        /// Penalty for ending a paragraph with a hyphen
        FinalHyphenDemerits : float
        /// Penalty multiplier for fitness class differences
        FitnessClassDifferencePenalty : float
    }

    /// Creates default options with standard TeX-like values
    static member Default (lineWidth : float) =
        {
            LineWidth = lineWidth
            Tolerance = 1.0
            AdjacentLooseTightDemerits = 10000.0
            DoubleHyphenDemerits = 10000.0
            FinalHyphenDemerits = 5000.0
            FitnessClassDifferencePenalty = 100.0
        }

module LineBreaker =
    /// Break a paragraph into lines using the Knuth-Plass algorithm.
    /// Returns a list of lines with their start/end positions and adjustment ratios.
    /// Raises an exception if no valid breaking is possible.
    let breakLines (options : LineBreakOptions) (items : Item list) : Line list = failwith "Not implemented"

module Items =
    /// Creates a box with the given width
    let box (width : float) : Item =
        Box
            {
                Width = width
            }

    /// Creates glue with the given width, stretch, and shrink values
    let glue (width : float) (stretch : float) (shrink : float) : Item =
        Glue
            {
                Width = width
                Stretch = stretch
                Shrink = shrink
            }

    /// Creates a penalty with the given width, cost, and flagged status
    let penalty (width : float) (cost : float) (flagged : bool) : Item =
        Penalty
            {
                Width = width
                Cost = cost
                Flagged = flagged
            }

    /// Creates a forced break (infinite penalty against not breaking)
    let forcedBreak () : Item = penalty 0.0 (-infinity) false

    /// Converts a simple string into a list of items (boxes for words, glue for spaces)
    let fromString (text : string) (wordWidth : string -> float) (spaceWidth : float) : Item list =
        let words = text.Split ([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)

        [
            for i, word in Array.indexed words do
                yield box (wordWidth word)

                // Add glue between words (but not after the last word)
                if i < words.Length - 1 then
                    yield glue spaceWidth (spaceWidth * 0.5) (spaceWidth * 0.333)
        ]
