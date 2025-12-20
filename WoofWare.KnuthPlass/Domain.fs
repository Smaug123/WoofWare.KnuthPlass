namespace WoofWare.KnuthPlass

/// Represents a fixed-width item (word, character, etc) for display.
type Box =
    {
        /// <summary>
        /// The width of this box.
        /// </summary>
        /// <remarks>
        /// The unit is arbitrary, but the motivating example is text layout; if this Box were a single
        /// word of text, then this Width would be "the sum of the display widths of the grapheme clusters in the word".
        ///
        /// The width may be zero or negative, but these "must be used with care and understanding",
        /// according to Knuth and Plass.
        /// </remarks>
        Width : float32
    }

    /// Terse but human-readable non-round-trip string representation.
    override this.ToString () : string = $"box[%0.02f{this.Width}]"

/// Represents whitespace to be displayed between boxes.
/// This can stretch or shrink as necessary within defined bounds to make the layout of the boxes pleasing.
type Glue =
    {
        /// <summary>The natural width of this glue when neither stretched nor compressed.</summary>
        /// <remarks>
        /// This can be negative, e.g. in the case of a backspace, indicating that the subsequent box
        /// should display *on top of* a previous box.
        ///
        /// The unit is arbitrary; it only matters that it's the same across all boxes, glues, and penalties.
        /// </remarks>
        Width : float32
        /// <summary>Maximum amount this glue can be stretched beyond its natural width.</summary>
        /// <remarks>
        /// The unit is arbitrary; it only matters that it's the same across all boxes, glues, and penalties.
        ///
        /// This can be negative; TODO explain the semantics
        /// </remarks>
        Stretch : float32
        /// <summary>Maximum amount this glue can be compressed below its natural width.</summary>
        /// <remarks>
        /// The unit is arbitrary; it only matters that it's the same across all boxes, glues, and penalties.
        ///
        /// This can be negative; TODO explain the semantics
        /// </remarks>
        Shrink : float32
    }

    /// Terse but human-readable non-round-trip string representation.
    override this.ToString () =
        $"glue[%0.02f{this.Width} / %0.02f{this.Stretch} / %0.02f{this.Shrink}]"

/// Represents a potential break point in the text, where we can end a line to begin a new line.
type Penalty =
    {
        /// <summary>
        /// Additional width added to the line just before the break if the penalty occurs (e.g. the width of a hyphen).
        /// </summary>
        /// <remarks>
        /// The unit is arbitrary; it only matters that it's the same across all boxes, glues, and penalties.
        /// </remarks>
        Width : float32
        /// <summary>
        /// Cost of breaking at this point. Negative values encourage breaks, positive values discourage them.
        /// </summary>
        /// <remarks>
        /// The unit is arbitrary; it only matters that it's the same across all boxes, glues, and penalties.
        /// </remarks>
        Cost : float32
        /// <summary>
        /// The algorithm tries to prevent two consecutive breaks at flagged penalties (e.g. to prevent two hyphenations
        /// in a row in the same word).
        /// </summary>
        /// <remarks>
        /// The unit is arbitrary; it only matters that it's the same across all boxes, glues, and penalties.
        /// </remarks>
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
        /// Index in the input items array of the first item that is set on this line
        Start : int
        /// Index-plus-one in the input items array of the last item that is set on this line.
        /// (That is, it's the endpoint of an exclusive interval.)
        End : int
        /// <summary>How much the glue in this line was stretched (positive) or compressed (negative).</summary>
        /// <remarks>
        /// An adjustment ratio is 0 if the line fit perfectly: glue didn't need to stretch or shrink.
        ///
        /// The adjustment ratio is positive, equal to (desired length - actual length) / (total glue stretchability),
        /// if the line was too short so we needed to stretch the glues.
        ///
        /// The adjustment ratio is negative, equal to (desired length - actual length) / (total glue shrinkability),
        /// if the line was too long so we needed to shrink the glues.
        ///
        /// Both total-stretchability and total-shrinkability must be strictly positive for this ratio to have any
        /// meaning; the paper does not define the adjustment ratio if stretching (resp. shrinking) was required and
        /// the total stretchability (resp. shrinkability) was nonpositive.
        ///
        /// The Knuth-Plass algorithm adjusts the width of all glues proportionally by adding to the glue
        /// a width of "adjustment ratio for line * stretch/shrinkability of that glue".
        /// </remarks>
        AdjustmentRatio : float32
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
        LineWidth : float32
        /// Badness threshold for applying the quadratic tolerance penalty (badness = 100 * |ratio|^3).
        /// Typical values: 1-3 for strict, 10 for moderate. Higher values delay the penalty and let looser lines survive.
        Tolerance : float32
        /// <summary>Added to badness in demerits calculation (tex.web:16901).</summary>
        /// <remarks>TeX's default is 10. The demerits formula is: (LinePenalty + badness)² + penalty²</remarks>
        LinePenalty : float32
        /// Penalty for consecutive lines of very different tightness (fitness diff > 1)
        AdjacentLooseTightDemerits : float32
        /// Penalty for consecutive hyphenated lines
        DoubleHyphenDemerits : float32
        /// Penalty for ending a paragraph with a hyphen
        FinalHyphenDemerits : float32
        /// <summary>Multiplier by which to penalise consecutive lines having different fitness classes.</summary>
        /// <remarks>See the <see cref="FitnessClass"/> type for what a fitness class is.
        /// Note: TeX only applies adj_demerits when fitness diff > 1, not for adjacent classes (diff=1).</remarks>
        FitnessClassDifferencePenalty : float32
        /// <summary>Zero-width glue providing baseline stretch for all lines (TeX's \rightskip).</summary>
        /// <remarks>
        /// This provides baseline stretchability so that single-word lines (which have no inter-word glue)
        /// get finite adjustment ratios instead of infinity. Without this, the algorithm treats all underfull
        /// single-word lines as equally bad, potentially preferring unnecessary hyphenation.
        ///
        /// Only the Stretch field is currently used; Width and Shrink have no effect.
        /// The Stretch should be positive to allow underfull lines to have meaningful badness values.
        /// </remarks>
        RightSkip : Glue
    }

    /// TeX's maximum badness value (inf_bad in tex.web:109). Per TeX's badness function
    /// (tex.web:16110-16113), this is returned directly when there's no stretch/shrink
    /// available (s <= 0) or when the ratio would be too extreme (r > 297 in TeX's units).
    static member internal infBad : float32 = 10000.0f

    /// Default tolerance value: matches TeX's default (200) which allows ratio up to ~1.26.
    /// TeX uses tolerance as a feasibility cutoff - lines with badness > tolerance are rejected.
    static member DefaultTolerance = 200.0f
    /// Default line penalty value: matches TeX's default (10).
    static member DefaultLinePenalty = 10.0f
    /// Default penalty for consecutive lines of very different tightness
    static member DefaultAdjacentLooseTightDemerits = 10000.0f
    /// Default penalty for consecutive hyphenated lines
    static member DefaultDoubleHyphenDemerits = 10000.0f
    /// Default penalty for ending a paragraph with a hyphen
    static member DefaultFinalHyphenDemerits = 5000.0f
    /// Default penalty for fitness class differences
    static member DefaultFitnessClassDifferencePenalty = 100.0f

    /// Creates default options with standard TeX values (RightSkip = 0).
    static member Default (lineWidth : float32) =
        {
            LineWidth = lineWidth
            Tolerance = LineBreakOptions.DefaultTolerance
            LinePenalty = LineBreakOptions.DefaultLinePenalty
            AdjacentLooseTightDemerits = LineBreakOptions.DefaultAdjacentLooseTightDemerits
            DoubleHyphenDemerits = LineBreakOptions.DefaultDoubleHyphenDemerits
            FinalHyphenDemerits = LineBreakOptions.DefaultFinalHyphenDemerits
            FitnessClassDifferencePenalty = LineBreakOptions.DefaultFitnessClassDifferencePenalty
            RightSkip =
                {
                    Width = 0.0f
                    Stretch = 0.0f
                    Shrink = 0.0f
                }
        }

    /// <summary>Creates default options with a custom RightSkip stretch value.</summary>
    /// <remarks>
    /// Setting <c>rightSkipStretch</c> to a positive value (e.g. 4.0) provides baseline stretchability
    /// for single-word lines, preventing all underfull single-word lines from being treated as equally
    /// bad. This can reduce unnecessary hyphenation of long words. TeX's default is 0.
    /// </remarks>
    static member DefaultWithRightSkip (lineWidth : float32) (rightSkipStretch : float32) =
        { LineBreakOptions.Default lineWidth with
            RightSkip =
                {
                    Width = 0.0f
                    Stretch = rightSkipStretch
                    Shrink = 0.0f
                }
        }

    /// <summary>Creates default options tuned for monospace layouts like a console where spaces can't
    /// really stretch.</summary>
    /// <remarks>
    /// This sets a high tolerance to accept underfull lines, and uses a RightSkip stretch of 4.0 to
    /// provide baseline stretchability for single-word lines (preventing unnecessary hyphenation).
    /// </remarks>
    static member DefaultMonospace (lineWidth : float32) =
        {
            LineWidth = lineWidth
            // Large tolerance so that we accept underfull lines.
            Tolerance = LineBreakOptions.infBad + 0.1f
            LinePenalty = LineBreakOptions.DefaultLinePenalty
            AdjacentLooseTightDemerits = LineBreakOptions.DefaultAdjacentLooseTightDemerits
            DoubleHyphenDemerits = LineBreakOptions.DefaultDoubleHyphenDemerits
            FinalHyphenDemerits = LineBreakOptions.DefaultFinalHyphenDemerits
            FitnessClassDifferencePenalty = LineBreakOptions.DefaultFitnessClassDifferencePenalty
            // RightSkip.Stretch = 4.0 provides baseline stretchability for single-word lines.
            // This prevents all underfull single-word lines from being treated as equally bad
            // (which would cause unnecessary hyphenation).
            // (Only Stretch is used; Width and Shrink have no effect.)
            RightSkip =
                {
                    Width = 0.0f
                    Stretch = 4.0f
                    Shrink = 0.0f
                }
        }
