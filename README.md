# Plutus Script Evaluation Test

## How to use

0. Create `.envrc.local` with the following content (adjust the paths as needed):
   ```sh
    export CARDANO_NODE_SOCKET_PATH="/home/yura/projects/cardano/node/node-state/sanchonet/node.sock"
    export CARDANO_NODE_CONFIG_PATH="/home/yura/projects/cardano/cardano-playground/docs/environments/sanchonet/config.json"
   ```
1. Enter the `nix` shell using either `nix develop` command or `direnv` hooked to your shell.
2. See available commands by entering `info` in the shell.
3. Run the script dump job using the `dump` command.

