# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectinshellargs
# See `shellArgs` in `mkHaskellProject` in ./project.nix for more details.

{ repoRoot
, inputs
, pkgs
, lib
, system
,
}:

# Each flake variant defined in your project.nix project will yield a separate
# shell. If no flake variants are defined, then cabalProject is the original project.
cabalProject: {
  name = "plutus-script-evaluation";

  packages = [
    pkgs.figlet
  ];

  scripts = {
    dump = {
      description = "Dump Plutus Script events from mainnet";
      group = "general";
      exec = ''
        cabal run dump-script-events -- \
          --mainnet \
          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
          --config "$CARDANO_NODE_CONFIG_PATH" \
          --events-per-file 5000 \
          --event-dir dumps/events \
          --checkpoint-dir dumps/checkpoints \
      '';
    };

    load = {
      description = "Load Plutus Script events into a database";
      group = "general";
      exec = ''
        cabal run load-script-events -- \
          --mainnet \
          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
          --config "$CARDANO_NODE_CONFIG_PATH" \
          --checkpoint-dir dumps/checkpoints \
          --database-conn-str "$DB_CONN_STRING" 
      '';
    };

    aggregate = {
      description = "Aggregate Plutus Script events from mainnet";
      group = "general";
      exec = ''
        cabal run aggregate-script-events -- --event-dir dumps/events
      '';
    };

    deserialise = {
      description = "Deserialise Plutus Scripts from mainnet";
      group = "general";
      exec = ''
        cabal run deserialise-scripts -- --database-conn-str "$DB_CONN_STRING"
      '';
    };

    materialise = {
      description = "Materialise database views";
      group = "general";
      exec = ''
        cabal run materialise-views -- --database-conn-str "$DB_CONN_STRING"
      '';
    };

    evaluate = {
      description = "Evaluate Plutus Scripts from mainnet";
      group = "general";
      exec = ''
        cabal run evaluate-scripts -- --database-conn-str "$DB_CONN_STRING"
      '';
    };
  };

  # env = {
  #   KEY = "VALUE";
  # };

  shellHook = ''
    figlet "Plutus Script Evaluation"
  '';

  preCommit = {
    cabal-fmt.enable = true;
    stylish-haskell.enable = false;
    fourmolu.enable = true;
    hlint.enable = true;
    editorconfig-checker.enable = true;
    nixpkgs-fmt.enable = false;
  };
}
