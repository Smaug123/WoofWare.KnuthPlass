# WoofWare.KnuthPlass Benchmarks

This project contains BenchmarkDotNet performance benchmarks for the Knuth-Plass line breaking algorithm.

**Note:** These benchmarks use `[<InProcess>]` mode to run in the host process without requiring BenchmarkDotNet to rebuild the project. This ensures compatibility with non-standard .NET installations (e.g., nixpkgs) while still providing JIT warmup, statistical analysis, and memory diagnostics.

## Running Benchmarks

### Run All Benchmarks

```bash
cd WoofWare.KnuthPlass.Benchmarks
dotnet run -c Release
```

**Important:** Always run benchmarks in Release mode (`-c Release`) for accurate performance measurements.

### Run Specific Benchmark Classes

```bash
# Run only LineBreakingBenchmarks
dotnet run -c Release --filter *LineBreakingBenchmarks*

# Run only LineWidthVariationBenchmarks
dotnet run -c Release --filter *LineWidthVariationBenchmarks*

# Run only RealWorldScenarioBenchmarks
dotnet run -c Release --filter *RealWorldScenarioBenchmarks*
```

### Run Specific Benchmark Methods

```bash
# Run only the BreakLines method
dotnet run -c Release --filter *BreakLines*

# Run only memory profiling benchmarks
dotnet run -c Release --filter *MemoryProfile*
```

## Benchmark Suites

### LineBreakingBenchmarks

Tests the core line breaking algorithm with varying paragraph sizes.

**Purpose:** Verify that the algorithm maintains O(N) time complexity as input size increases.

**Key Metrics:**
- Execution time (should scale linearly)
- Memory allocations
- Gen0/Gen1/Gen2 garbage collections

### LineWidthVariationBenchmarks

Tests how different line widths (50, 100, 150, 200 units) affect performance with a fixed 1000-word paragraph.

**Purpose:** Understand how output constraints impact algorithm performance.

### RealWorldScenarioBenchmarks

Tests realistic text scenarios with:
- Varying word widths (3.0 to 8.0 units)
- Stretchable/shrinkable glue
- Hyphenation penalties (10% of positions)

**Purpose:** Measure performance on more realistic text processing scenarios.

## Understanding Results

BenchmarkDotNet produces detailed reports including:

- **Mean**: Average execution time
- **Error**: Half of 99.9% confidence interval
- **StdDev**: Standard deviation of all measurements
- **Allocated**: Total memory allocated per operation
- **Gen0/Gen1/Gen2**: Garbage collection counts

### Example Output

```
| Method     | WordCount | Mean      | Error    | StdDev   | Allocated |
|----------- |---------- |----------:|---------:|---------:|----------:|
| BreakLines | 100       |  12.34 μs | 0.123 μs | 0.109 μs |   15.2 KB |
| BreakLines | 500       |  61.23 μs | 0.456 μs | 0.427 μs |   76.8 KB |
| BreakLines | 1000      | 122.45 μs | 0.789 μs | 0.738 μs |  153.6 KB |
| BreakLines | 5000      | 612.34 μs | 3.456 μs | 3.234 μs |  768.0 KB |
```

## Expected results

The main algorithm should have time scaling linearly with input size.
