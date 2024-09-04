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

  packages = [ pkgs.figlet ];

  scripts = {
    dump = {
      description = "Dump Plutus Script events from mainnet";
      group = "general";
      exec = ''
        cabal run dump-script-events -- \
          --mainnet \
          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
          --config "$CARDANO_NODE_CONFIG_PATH" \
          --events-per-file 10000 \
          --dump-dir dumps \
          --checkpoint-dir dumps/checkpoints \
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
    nixpkgs-fmt.enable = true;
  };
}
