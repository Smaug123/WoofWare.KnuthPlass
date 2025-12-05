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
    /// A content box, e.g. a word, word-fragment, or character.
    | Box of Box
    /// Whitespace that can stretch or shrink.
    | Glue of Glue
    /// A potential break point in the text (e.g. somewhere to add a hyphen).
    | Penalty of Penalty

    /// Terse but human-readable non-round-trip string representation.
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

    /// Terse but human-readable non-round-trip string representation.
    override this.ToString () =
        $"%i{this.Start}..=%i{this.End - 1} (%0.02f{this.AdjustmentRatio})"

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
        /// Badness threshold for applying the quadratic tolerance penalty (badness = 100 * |ratio|^3).
        /// Typical values: 1-3 for strict, 10 for moderate. Higher values delay the penalty and let looser lines survive.
        Tolerance : float
        /// <summary>Added to badness in demerits calculation (tex.web:16901).</summary>
        /// <remarks>TeX's default is 10. The demerits formula is: (LinePenalty + badness)² + penalty²</remarks>
        LinePenalty : float
        /// Penalty for consecutive lines of very different tightness (fitness diff > 1)
        AdjacentLooseTightDemerits : float
        /// Penalty for consecutive hyphenated lines
        DoubleHyphenDemerits : float
        /// Penalty for ending a paragraph with a hyphen
        FinalHyphenDemerits : float
        /// <summary>Multiplier by which to penalise consecutive lines having different fitness classes.</summary>
        /// <remarks>See the <see cref="FitnessClass"/> type for what a fitness class is.
        /// Note: TeX only applies adj_demerits when fitness diff > 1, not for adjacent classes (diff=1).</remarks>
        FitnessClassDifferencePenalty : float
    }

    /// TeX's maximum badness value (inf_bad in tex.web:109). Per TeX's badness function
    /// (tex.web:16110-16113), this is returned directly when there's no stretch/shrink
    /// available (s <= 0) or when the ratio would be too extreme (r > 297 in TeX's units).
    static member internal infBad = 10000.0

    /// Default tolerance value: matches TeX's default (200) which allows ratio up to ~1.26.
    /// TeX uses tolerance as a feasibility cutoff - lines with badness > tolerance are rejected.
    static member DefaultTolerance = 200.0
    /// Default line penalty value: matches TeX's default (10).
    static member DefaultLinePenalty = 10.0
    /// Default penalty for consecutive lines of very different tightness
    static member DefaultAdjacentLooseTightDemerits = 10000.0
    /// Default penalty for consecutive hyphenated lines
    static member DefaultDoubleHyphenDemerits = 10000.0
    /// Default penalty for ending a paragraph with a hyphen
    static member DefaultFinalHyphenDemerits = 5000.0
    /// Default penalty for fitness class differences
    static member DefaultFitnessClassDifferencePenalty = 100.0

    /// Creates default options with standard TeX-like values
    static member Default (lineWidth : float) =
        {
            LineWidth = lineWidth
            Tolerance = LineBreakOptions.DefaultTolerance
            LinePenalty = LineBreakOptions.DefaultLinePenalty
            AdjacentLooseTightDemerits = LineBreakOptions.DefaultAdjacentLooseTightDemerits
            DoubleHyphenDemerits = LineBreakOptions.DefaultDoubleHyphenDemerits
            FinalHyphenDemerits = LineBreakOptions.DefaultFinalHyphenDemerits
            FitnessClassDifferencePenalty = LineBreakOptions.DefaultFitnessClassDifferencePenalty
        }

    /// Creates default options with standard TeX-like values, tuned for monospace layouts like a console where
    /// spaces can't really stretch.
    static member DefaultMonospace (lineWidth : float) =
        {
            LineWidth = lineWidth
            // Large tolerance so that we accept underfull lines.
            Tolerance = LineBreakOptions.infBad + 0.1
            LinePenalty = LineBreakOptions.DefaultLinePenalty
            AdjacentLooseTightDemerits = LineBreakOptions.DefaultAdjacentLooseTightDemerits
            DoubleHyphenDemerits = LineBreakOptions.DefaultDoubleHyphenDemerits
            FinalHyphenDemerits = LineBreakOptions.DefaultFinalHyphenDemerits
            FitnessClassDifferencePenalty = LineBreakOptions.DefaultFitnessClassDifferencePenalty
        }
