Plutus Scripts Evaluation
===

Tools used to:
1. Accumulate Plutus script evaluation events by replaying blockchain folding over the Ledger state and extracting `PlutusScriptEvaluationEvent`s.
2. Record accumulated events:
    1. On the file system as "dump" files.
    2. In the PostgreSQL database.

## How to use

0. Initialise the PostgreSQL database and connection using files in the `database` folder:
   * There is a [pgModeler](https://pgmodeler.io/) (Open-source tool) project for it,
   * As well as the DDL statements.
1. Create `.envrc.local` with the following content (adjust the paths as needed):
   ```sh
    export CARDANO_NODE_SOCKET_PATH="/home/projects/cardano/node/node-state/mainnet/node.sock"
    export CARDANO_NODE_CONFIG_PATH="/home/projects/cardano/playground/docs/environments/mainnet/config.json"
    export DB_CONN_STRING="dbname=mainnet_plutus_events"
   ```
2. Enter the `nix` shell using either `nix develop` command or `direnv` hooked to your shell.
3. See available commands by entering `info` in the shell.
4. Run the script dump job using the `dump` command or script upload job with the `load` command.
