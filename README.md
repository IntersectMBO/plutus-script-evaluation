# Plutus Scripts Evaluation

Tools used to:

1. Accumulate Plutus script evaluation events by replaying blockchain folding over the Ledger state and extracting `PlutusScriptEvaluationEvent`s.
2. Record accumulated events:
   1. On the file system as "dump" files.
   2. In the PostgreSQL database.

## How to use

0. Initialise the PostgreSQL database and connection using files in the `database` folder:
   - There is a [pgModeler](https://pgmodeler.io/) (Open-source tool) project for it
   - As well as the DDL statements
1. Create `.envrc.local` with the following content (adjust the paths as needed):

   ```sh
   export CARDANO_NODE_SOCKET_PATH="/home/projects/cardano/node/node-state/mainnet/node.sock"
   export CARDANO_NODE_CONFIG_PATH="/home/projects/cardano/playground/docs/environments/mainnet/config.json"
   export DB_CONN_STRING="dbname=mainnet_plutus_events"
   ```
2. Enter the `nix` shell using either `nix develop` command or `direnv` hooked to your shell.
3. See available commands by entering `info` in the shell.
4. Run the script dump job using the `dump` command or script upload job with the `load` command.

## How to re-evaluate recorded Plutus Script evaluations locally

The database contains plutus script evaluation events from Mainnet which can be replayed locally to re-evaluate the scripts.

There is less value in re-evaluating scripts without any changes, as one would
simply re-obtain results that are already known. However, this can be useful
when the script evaluation logic has changed, and one wants to compare results
produced by the new logic with the old results.

The repository contains a program that can be used to re-evaluate the scripts
locally. You can use this program as a basis for your own re-evaluation, where
you can modify various parameters to suit your needs:

- The [Main module](run-script-evaluations/src/Main.hs) of the `run-script-evaluations` executable.
- The main workhorse, function `evaluateScripts` in the [Evaluation module](run-script-evaluations/src/Evaluate.hs) does the boring parts (aggregating relevant script evaluation inputs, streaming the data from DB to a local computer, decoding CBOR, forking worker threads) so that you can do the interesting part: traverse script evaluations from the Mainnet accessing all of the original evaluation inputs to re-interpret them accordingly to your task.

## Github Actions Workflow

Re-evaluates the Plutus script events from Mainnet periodically (once a week) to test compatibility with the latest Plutus development.

The workflow is defined in the `.github/workflows/evaluate.yml` file.

### Workflow Overview

The workflow runs:

- **Scheduled**: Every Saturday at midnight (UTC)
- **Manual**: Via workflow dispatch with optional `startFrom` parameter
- **Environment**: Self-hosted runner on `plutus-node` server with database access

### Workflow Steps

1. **Checkout Repository**: Uses the latest code from this repository
2. **Update Plutus Dependency**: Executes `run-script-evaluations/add_srp.sh` to:
   - Fetch the latest commit from `IntersectMBO/plutus` master branch
   - Generate a `source-repository-package` entry with the latest commit hash and nix SHA
   - Update `cabal.project` with the new dependency (removing any existing entries for idempotency)
3. **Run Evaluations**: Builds and executes the `run-script-evaluations` program:
   - Uses `nix develop` environment for reproducible builds
   - Connects to the local PostgreSQL database containing Mainnet script events
   - Processes scripts starting from the specified record (default: 0)
   - Outputs evaluation results to `evaluation.log`

### Script Management Tool

The `add_srp.sh` script provides robust dependency management with:

- **Command-line options**: `--branch <branch>` (default: master), `--quiet` for CI
- **Error handling**: Validates dependencies (`jq`, `nix-prefetch-git`) and fetched data
- **Idempotent operation**: Safely removes existing entries before adding new ones
- **Atomic updates**: Uses backup/restore mechanism to prevent file corruption
- **Exit codes**: Returns specific codes for different error conditions (0-5)

### Usage Examples

```bash
# Default behavior (master branch)
./run-script-evaluations/add_srp.sh

# Use specific branch
./run-script-evaluations/add_srp.sh --branch develop

# Quiet mode for CI
./run-script-evaluations/add_srp.sh --quiet

# Combined options
./run-script-evaluations/add_srp.sh --branch main --quiet
```

This automated testing ensures that script evaluations remain compatible as Plutus evolves, providing early detection of breaking changes or performance regressions.
