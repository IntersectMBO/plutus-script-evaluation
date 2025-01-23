# Plutus Scripts Evaluation

Tools used to:

1. Accumulate Plutus script evaluation events by replaying blockchain folding over the Ledger state and extracting `PlutusScriptEvaluationEvent`s.
2. Record accumulated events:
   1. On the file system as "dump" files.
   2. In the PostgreSQL database.

## How to use

0. Initialise the PostgreSQL database and connection using files in the `database` folder:
   - There is a [pgModeler](https://pgmodeler.io/) (Open-source tool) project for it,
   - As well as the DDL statements.
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

- The [Main module](plutus-script-evaluation/evaluate-scripts/Main.hs) of the `evaluate-scripts` executable.
- The main workhorse, function `evaluateScripts` in the [Evaluation module](plutus-script-evaluation/evaluate-scripts/Evaluation.hs) does the boring parts (aggregating relevant script evaluation inputs, streaming the data from DB to a local computer, decoding CBOR) so that you can do the interesting part: fold over the script evaluations from the Mainnet accessing all of the original evaluation inputs, to re-interpret them accordingly to your task, maintaining local state (accumulator) if needed. 
