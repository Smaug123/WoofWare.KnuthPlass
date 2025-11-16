namespace WoofWare.KnuthPlass

/// Represents a fixed-width item (word, character, etc.)
type Box =
    {
        /// <summary>
        /// The width of this box.
        /// </summary>
        /// <remarks>
        /// All widths in the system are relative, but the motivating example is text layout; if this Box were a single
        /// word of text, then this Width would be "the sum of the display widths of the grapheme clusters in the word".
        /// </remarks>
        Width : float
    }

/// Represents stretchable/shrinkable whitespace.
type Glue =
    {
        /// <summary>The natural width of this glue when neither stretched nor compressed.</summary>
        Width : float
        /// <summary>Maximum amount this glue can be stretched beyond its natural width.</summary>
        Stretch : float
        /// <summary>Maximum amount this glue can be compressed below its natural width.</summary>
        Shrink : float
    }

/// Represents a potential break point in the text.
type Penalty =
    {
        /// Additional width added to the line just before the break if the penalty occurs (e.g. the width of a hyphen).
        Width : float
        /// Cost of breaking at this point. Negative values encourage breaks, positive values discourage them.
        Cost : float
        /// The algorithm tries to prevent two consecutive breaks at flagged penalties (e.g. to prevent two hyphenations
        /// in a row).
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

/// <summary>Categorises how tight or loose a line is, based on how much the spaces need to be stretched or shrunk to
/// justify the line.</summary>
/// <remarks>
/// The point is that a transition from a line with very wide spacing to a line with very cramped spacing is
/// visually jarring, so the algorithm penalises it.
/// </remarks>
type FitnessClass =
    | Tight = 0
    | Normal = 1
    | Loose = 2
    | VeryLoose = 3

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
        /// <summary>Multiplier by which to penalise consecutive lines having different fitness classes.</summary>
        /// <remarks>See the <see cref="FitnessClass"/> type for what a fitness class is.</remarks>
        FitnessClassDifferencePenalty : float
    }

    /// Creates default options with standard TeX-like values
    static member Default (lineWidth : float) =
        {
            LineWidth = lineWidth
            Tolerance = 10.0 // Allow significant stretch/shrink
            AdjacentLooseTightDemerits = 10000.0
            DoubleHyphenDemerits = 10000.0
            FinalHyphenDemerits = 5000.0
            FitnessClassDifferencePenalty = 100.0
        }
