# value-statistics

A tool for analyzing Plutus `Value` structures from script contexts stored in the Cardano mainnet database.

## Overview

`value-statistics` collects comprehensive statistics on:
- **Policies per Value**: Distribution of unique currency symbols
- **Tokens per Value**: Distribution of token counts (policy + token name pairs)
- **Token Quantities**: Distribution by power-of-2 boundaries, overflow detection

## Features

- **Flexible Data Collection**:
  - Random sampling for quick analysis
  - Full scan for comprehensive statistics
  - Report-only mode: Generate reports from checkpoints without database access
- **Checkpoint/Resume**: Save progress during long-running full scans
- **Graceful Interruption**: Clean Ctrl+C handling with automatic checkpoint saving
- **Human-Readable Checkpoints**: Pretty-printed JSON format for easy inspection
- **Cumulative Progress Tracking**: Shows overall progress when resuming from checkpoints
- **Multi-Version Support**: Works with Plutus V1, V2, and V3
- **Streaming Processing**: Memory-efficient for large datasets
- **Fail-Fast**: Stops on parse errors with detailed diagnostics

## Usage

### Quick Start

```bash
# Sample 1% of records (fast, approximate results)
cabal run value-statistics -- \
  --database-conn-str "host=plutus dbname=mainnet_plutus_events user=plutus_reader" \
  --sample-percent 1.0

# Full scan with checkpointing (comprehensive, resumable)
cabal run value-statistics -- \
  --database-conn-str "host=plutus dbname=mainnet_plutus_events user=plutus_reader" \
  --checkpoint-file ./checkpoint.json \
  --text-output ./report.txt
```

### Command-Line Options

| Option | Required | Description |
|--------|----------|-------------|
| `--database-conn-str CONN_STR` | Conditional* | PostgreSQL connection string |
| `--sample-percent PERCENT` | No | Percentage to sample (1-100). Omit for full scan |
| `--checkpoint-file FILE` | Conditional* | JSON checkpoint file for resuming full scans or report-only mode |
| `--text-output FILE` | No | Path to save plain text report |

\* **Either** `--database-conn-str` **or** `--checkpoint-file` must be provided

### Connection String Format

PostgreSQL connection string syntax:

```bash
# Basic format
"host=HOST dbname=DATABASE user=USERNAME password=PASSWORD"

# Using environment variables
"host=localhost dbname=mainnet_plutus_events user=$USER"

# With additional parameters
"host=plutus dbname=mainnet_plutus_events user=reader connect_timeout=10"
```

See [PostgreSQL documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING) for full syntax.

## Collection Modes

### Sample Mode

Use `--sample-percent` to analyze a random sample of rows using PostgreSQL's `TABLESAMPLE SYSTEM`:

```bash
# Sample 5% of records
--sample-percent 5.0
```

**Characteristics**:
- Fast: Analyzes a fraction of the dataset
- Page-level sampling: PostgreSQL samples entire data pages
- Approximate results: Statistical estimate of full dataset
- No checkpointing: Runs to completion or fails

**When to use**:
- Quick exploratory analysis
- Testing queries or workflows
- Approximate distributions are sufficient

### Full Scan Mode

Omit `--sample-percent` to process all rows:

```bash
# Full scan without checkpointing (not recommended)
--database-conn-str "..."

# Full scan with checkpointing (recommended)
--checkpoint-file ./stats-checkpoint.json
```

**Characteristics**:
- Complete: Analyzes entire dataset
- Resumable: Can continue after interruption (press Ctrl+C to save and exit)
- Checkpoint every 100,000 rows automatically
- Graceful cleanup: Properly closes database connections on interrupt
- Exact results: Precise statistics

**When to use**:
- Production analysis requiring exact numbers
- Long-running jobs (hours to days)
- When resume capability is important

### Report-Only Mode

Generate reports from existing checkpoint files without database access:

```bash
# Terminal output only
cabal run value-statistics -- \
  --checkpoint-file ./stats-checkpoint.json

# With text file export
cabal run value-statistics -- \
  --checkpoint-file ./stats-checkpoint.json \
  --text-output ./report.txt
```

**Characteristics**:
- No database required: Works offline or on machines without DB access
- Instant: Generates report immediately from checkpoint data
- Multiple formats: Same checkpoint can generate many reports
- Portable: Share checkpoint files to generate reports elsewhere

**When to use**:
- Regenerating reports after checkpoint collection
- Sharing analysis results with team members without DB access
- Creating multiple report formats from same data
- Archival purposes (checkpoint files are human-readable JSON)

## Checkpointing

### How It Works

During full scans with `--checkpoint-file`, the tool:
1. Saves progress every 100,000 rows
2. Stores: accumulator state + last processed pk + timestamp
3. Resumes from last pk on next run

### Checkpoint File Format

Checkpoints are saved as pretty-printed JSON:

```json
{
  "checkpointLastPk": 12340000,
  "checkpointTimestamp": "2025-10-16T15:30:42Z",
  "checkpointAccumulator": {
    "saCount": 1234000,
    "saPolicyMin": 0,
    "saPolicyMax": 487,
    "saPolicySum": 2468000,
    "saPolicyDistribution": {
      "1": 850000,
      "2": 280000,
      "3": 104000
    },
    "saTokenDistribution": { ... },
    "saQuantityBoundaries": { ... },
    ...
  }
}
```

### Resuming from Checkpoint

If a checkpoint file exists, the tool automatically resumes with **cumulative progress tracking**:

```bash
# First run (interrupted after 1M rows)
cabal run value-statistics -- \
  --database-conn-str "..." \
  --checkpoint-file ./checkpoint.json

# Output:
# Starting fresh (checkpoint file: ./checkpoint.json)
# Total script contexts: 50000000
# Already processed: 0
# Remaining to process: 50000000
# ...
# Processing pk 1000000 (1000000 / 50000000 = 2.00%)
# ^C
#
# Interrupted. Saving checkpoint...
# Checkpoint saved at pk 1000000 to: ./checkpoint.json
# Checkpoint saved. Run again to resume from this point.

# Second run (resumes from pk 1000000 with cumulative progress)
cabal run value-statistics -- \
  --database-conn-str "..." \
  --checkpoint-file ./checkpoint.json

# Output:
# Loaded checkpoint from ./checkpoint.json (saved at 2025-10-16T15:30:42Z, last pk: 1000000)
# Resuming from checkpoint (last pk: 1000000)
# Total script contexts: 50000000
# Already processed: 1000000
# Remaining to process: 49000000
# ...
# Processing pk 1010000 (1010000 / 50000000 = 2.02%)
# ...
```

**Note**: Progress percentage shows **cumulative progress** (total processed / total count), not just current batch progress. This makes it easy to track overall completion across multiple sessions.

## Output

### Terminal Output

The tool prints:
- Mode and configuration
- Total row count and max pk
- Progress updates (every 1,000 rows)
- Checkpoint save notifications (every 100,000 rows)
- Final statistics report

Example progress output:

```
Sample mode:
Processing pk 253010000 (1%)
Processing pk 254500000 (3%)

Full scan mode:
Processing pk 12345000 / 50000000 (24%)
Checkpoint saved at pk 12340000 to: ./checkpoint.json
```

### Statistics Report

The report includes:

1. **Policies per Value**
   - Min, Max, Mean
   - Percentiles: P50, P90, P95, P99
   - Histogram (0-9: individual bins, 10-500: bins of 10)

2. **Tokens per Value**
   - Min, Max, Mean
   - Percentiles: P50, P90, P95, P99
   - Histogram (same binning as policies)

3. **Token Quantities by Power-of-2 Boundaries**
   - Distribution across ranges: `< -2^127`, `[-2^127, -2^64)`, ..., `> 2^127`
   - Overflow detection: quantities ≥ 99% of 2^64 and 2^128
   - Percentage of total quantities in each range

Example output:

```
=====================================================================
                   Value Statistics Report
=====================================================================

Total Values analyzed: 1234567

---------------------------------------------------------------------
Policies per Value:
---------------------------------------------------------------------
Min:  1
Max:  487
Mean: 2.45

Percentiles:
  P50:  2
  P90:  4
  P95:  6
  P99:  15

Distribution (0-9: individual bins, 10-500: bins of 10):
  1       :     850000 ##################################################
  2       :     280000 ################
  3       :     104000 ######
  ...
```

### Text Report Export

Use `--text-output` to save the report to a file:

```bash
--text-output ./value-stats-report.txt
```

The file contains the same information as terminal output, suitable for:
- Archiving analysis results
- Sharing with team members
- Feeding into other analysis tools

## Examples

### Quick Sample Analysis

Analyze 1% of records with report export:

```bash
cabal run value-statistics -- \
  --database-conn-str "host=plutus dbname=mainnet_plutus_events user=reader" \
  --sample-percent 1.0 \
  --text-output ./sample-report.txt
```

### Production Full Scan

Complete analysis with checkpointing:

```bash
cabal run value-statistics -- \
  --database-conn-str "host=plutus dbname=mainnet_plutus_events user=reader" \
  --checkpoint-file ./production-checkpoint.json \
  --text-output ./full-report.txt
```

### Resume After Interruption

Simply run the same command again - the tool detects and resumes from the checkpoint:

```bash
# Same command as above - automatically resumes with cumulative progress
cabal run value-statistics -- \
  --database-conn-str "host=plutus dbname=mainnet_plutus_events user=reader" \
  --checkpoint-file ./production-checkpoint.json \
  --text-output ./full-report.txt
```

**Interruption safety**: Press Ctrl+C at any time during a full scan. The tool will:
1. Print "Interrupted. Saving checkpoint..."
2. Save current progress to checkpoint file
3. Cleanly close database connection (no libpq errors)
4. Exit gracefully

Run the same command again to resume where you left off.

### Generate Report Without Database

Once you have a checkpoint file, regenerate reports anytime without database access:

```bash
# View report on terminal
cabal run value-statistics -- \
  --checkpoint-file ./production-checkpoint.json

# Export to text file
cabal run value-statistics -- \
  --checkpoint-file ./production-checkpoint.json \
  --text-output ./latest-report.txt

# Share checkpoint with team members - they can generate reports without DB credentials
scp ./production-checkpoint.json team-member@remote:/path/
```

### Different Sample Sizes

Compare results at different sampling rates:

```bash
# 0.1% sample (very fast, rough estimate)
--sample-percent 0.1 --text-output ./p0.1-report.txt

# 1% sample (fast, good estimate)
--sample-percent 1.0 --text-output ./p1-report.txt

# 10% sample (slower, accurate estimate)
--sample-percent 10.0 --text-output ./p10-report.txt
```

## Performance

### Sample Mode

- **0.1% sample**: ~1-5 minutes (depending on data distribution)
- **1% sample**: ~5-30 minutes
- **10% sample**: ~30-180 minutes

### Full Scan Mode

- **Complete dataset**: Hours to days (depends on total row count)
- **Checkpoint overhead**: Minimal (~1-2% slowdown)
- **Resume startup**: <1 second (loads checkpoint from JSON)

### Memory Usage

- Constant memory usage (~50-100 MB)
- Streaming architecture processes one row at a time
- Accumulator size grows with distribution complexity, not row count

## Error Handling

### Parse Failures

The tool uses fail-fast error handling. If a script context cannot be parsed:

```
ERROR: Failed to parse script context at pk=12345678 with ledger language PlutusV2
```

The tool exits immediately with:
- pk of the failing row
- Ledger language (PlutusV1/V2/V3)
- SQL error details if applicable

### Database Connection Issues

Connection errors display PostgreSQL diagnostics:

```
SQL State: 08006
SQL Exec Status: PGRES_FATAL_ERROR
SQL Error Message: could not connect to server
SQL Error Detail: ...
SQL Error Hint: ...
```

### Checkpoint File Issues

If a checkpoint file is corrupted or incompatible:

```
Could not load checkpoint: Error in $: key "checkpointAccumulator" not found
Starting fresh (checkpoint file: ./checkpoint.json)
```

The tool starts a new analysis, overwriting the corrupted checkpoint.

## Database Schema Requirements

The tool expects these tables:

### `script_evaluation_events`

| Column | Type | Description |
|--------|------|-------------|
| `pk` | `INT64` | Primary key |
| `script_hash` | `BYTEA` | Hash of script |
| `script_context` | `BYTEA` | Serialized script context |

### `serialised_scripts`

| Column | Type | Description |
|--------|------|-------------|
| `hash` | `BYTEA` | Script hash (FK) |
| `ledger_language` | `INT` | Plutus version (1/2/3) |

The tool joins these tables to get script contexts with their ledger language.

## Troubleshooting

### "No such table: script_evaluation_events"

Ensure you're connected to the correct database with mainnet data.

### "TABLESAMPLE clause can only be applied to tables"

This shouldn't occur as we query the base table. If it does, the database schema may have changed.

### Slow Progress During Full Scan

Full scans are inherently slow. Use checkpointing to enable safe interruption:
- Stop with Ctrl+C
- Resume later from checkpoint
- Monitor progress percentage

### Large Checkpoint Files

Checkpoint files grow with distribution complexity:
- Typical size: 10-100 KB
- Large size (>1 MB): Many unique policy/token counts
- Not a problem: JSON is human-readable and compresses well

## Development

### Building

```bash
cabal build value-statistics
```

### Testing

```bash
# Test with small sample
cabal run value-statistics -- \
  --database-conn-str "..." \
  --sample-percent 0.01
```

### Modifying

Key modules:
- `Main.hs`: Entry point, database queries, collection logic
- `ValueStats.hs`: Statistics accumulation, reporting, checkpointing
- `Options.hs`: CLI argument parsing

## License

See repository root for license information.
