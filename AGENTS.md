# AGENTS.md

This file provides guidance to agents such as Claude or Codex when working with code in this repository.

## Project Overview

This is an F# implementation of the Knuth-Plass line breaking algorithm, a sophisticated text layout algorithm used in TeX. The algorithm finds optimal line breaks in paragraphs by minimizing a global "badness" function using dynamic programming.

## Architecture

The codebase follows a layered design from primitives to high-level API:

1. **Domain.fs** - Core types: `Box` (fixed-width items), `Glue` (stretchable/shrinkable whitespace), `Penalty` (break points with costs), `Line` (output), `FitnessClass`, `LineBreakOptions`
2. **Items.fs** - Constructors for items (`Items.box`, `Items.glue`, `Items.penalty`, `Items.forcedBreak`), hyphenation support (`wordFromFragments`), and glue presets (`defaultGlue`, `monospaceGlue`)
3. **LineBreaker.fs** - The algorithm itself. `LineBreaker.breakLines` implements a two-pass Knuth-Plass algorithm with TeX's "artificial demerits" rescue mechanism
4. **Text.fs** - High-level string API. `Text.format` takes callbacks for word measurement and hyphenation, handles paragraph splitting

### Algorithm (LineBreaker module)

The `breakLines` function implements the Knuth-Plass algorithm using dynamic programming:
- Precomputes cumulative sums for efficient line computation
- Tracks best break nodes at each position for each fitness class (Tight/Normal/Loose/VeryLoose)
- Computes demerits based on badness, penalties, and fitness class mismatches
- Backtracks to recover the optimal solution

## Development Commands

### Building
```bash
dotnet build
```

### Running Tests
```bash
# Run all tests
dotnet test

# Run specific test (correct NUnit filtering)
dotnet build
dotnet woofware.nunittestrunner WoofWare.KnuthPlass.Test/bin/Debug/net9.0/WoofWare.KnuthPlass.Test.dll --filter "FullyQualifiedName~TestName"
```

### Formatting
```bash
dotnet fantomas .
```

### Analyzers (requires Nix)
```bash
./analyzers/run.sh
```

### Benchmarks
```bash
dotnet run -c Release --project WoofWare.KnuthPlass.Benchmarks
```

### Debug Tracing
Set environment variable `WOOFWARE_KNUTH_PLASS_DEBUG=1` to enable detailed algorithm tracing to stderr (DEBUG builds only).

Always run `dotnet fantomas .` and `./analyzers/run.sh` before committing.

# NUnit bugs

NUnit's filtering is pretty borked.
You can't apply filters that contain special characters in the test name (like a space character).
You have to do e.g. `FullyQualifiedName~singleword` rather than `FullyQualifiedName~single word test`, but this only works on tests whose names are single words to begin with.

Instead of running `dotnet test`, you can perform a build (`dotnet build`) and then run `dotnet woofware.nunittestrunner WoofWare.KnuthPlass.Test/bin/Debug/net9.0/WoofWare.KnuthPlass.Test.dll`.
This is an NUnit test runner which accepts a `--filter` arg that takes the same filter syntax as `dotnet test`, but actually parses it correctly: test names can contain spaces.
(The most foolproof way to provide test names to WoofWare.NUnitTestRunner is by XML-encoding: e.g. `FullyQualifiedName="MyNamespace.MyTestsClass&lt;ParameterType1%2CParameterType2&gt;.MyTestMethod"`. The `~` query operator is also supported.)

## FsCheck properties

Use unit-returning `actual |> shouldEqual expected` (with an `FsUnitTyped` assertion) rather than `actual = expected` as the conclusion of an FsCheck property.
When that property fails, the version which throws gives you much better output than the one with an equality check.

## Constructing FsCheck properties

Generally try and avoid filtering unless it's really hard to avoid.
You can often construct what you need instead of filtering.
For example, instead of filtering to a nonempty list, you can pass an `'a` and an `'a list`, and as the first line of the function, prepend the element to the list. Now it's nonempty!

## WoofWare.Expect snapshot testing

The usual workflow for updating snapshots using the WoofWare.Expect snapshot testing library is:

* Enter bulk-update mode by setting a `[<OneTimeSetUp>]` function (from NUnit.Framework) to `GlobalBuilderConfig.enterBulkUpdateMode ()`
* Run the tests. They will fail in the process of updating snapshots (this is so that you can't accidentally commit a test in update mode).
* Undo bulk-update mode by commenting out the `enterBulkUpdateMode ()`.
* Rerun the tests, if you like, to observe that the snapshots are now working.

