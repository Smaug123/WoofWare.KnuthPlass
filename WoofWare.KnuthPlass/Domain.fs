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

    /// Terse but human-readable non-round-trip string representation.
    override this.ToString () : string = $"box[%0.02f{this.Width}]"

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

    /// Terse but human-readable non-round-trip string representation.
    override this.ToString () =
        $"glue[%0.02f{this.Width} / %0.02f{this.Stretch} / %0.02f{this.Shrink}]"

/// Represents a potential break point in the text.
type Penalty =
    {
        /// Additional width added to the line just before the break if the penalty occurs (e.g. the width of a hyphen).
        Width : float
        /// Cost of breaking at this point. Negative values encourage breaks, positive values discourage them.
        Cost : float
        /// The algorithm tries to prevent two consecutive breaks at flagged penalties (e.g. to prevent two hyphenations
        /// in a row in the same word).
        Flagged : bool
    }

    /// Terse but human-readable non-round-trip string representation.
    override this.ToString () =
        let flagged = if this.Flagged then 'F' else '_'
        $"pen%c{flagged}[%0.02f{this.Width} cost %0.02f{this.Cost}]"

/// An item in the paragraph to be broken into lines
type Item =
    | Box of Box
    | Glue of Glue
    | Penalty of Penalty

    override this.ToString () =
        match this with
        | Item.Box box -> box.ToString ()
        | Item.Glue glue -> glue.ToString ()
        | Item.Penalty penalty -> penalty.ToString ()

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
    /// A line which has cramped spacing (adjustment ratio less than -1/2). An overfull line would
    /// be an extreme example of this class, where the adjustment ratio was less than -1: that would indicate
    /// every glue was being squashed as much as possible and we still had too much content in the line.
    | Tight = 0
    /// A line which has normal spacing, adjustment ratio between -1/2 and 1/2. (This line is possibly more cramped,
    /// possibly more loose than "ideal", but the glue is being squashed or stretched within normal bounds).
    | Normal = 1
    /// A line which has loose spacing, adjustment ratio between 1/2 and 1. (This line is more loose than it wants
    /// to be, but the glue is still being stretched within its allowed bounds.)
    | Loose = 2
    /// A line which is underfull: even after stretching every glue as much as possible, we're still not taking enough
    /// room.
    | VeryLoose = 3

/// Options for line breaking
type LineBreakOptions =
    {
        /// Target width for each line
        LineWidth : float
        /// Maximum allowed badness for a line (badness = 100 * |ratio|^3).
        /// Typical values: 1-3 for strict, 10 for moderate. Higher values allow looser lines.
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
            Tolerance = 10.0 // Moderate: allows ratio up to ~0.46
            AdjacentLooseTightDemerits = 10000.0
            DoubleHyphenDemerits = 10000.0
            FinalHyphenDemerits = 5000.0
            FitnessClassDifferencePenalty = 100.0
        }
