# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectinshellargs
# See `shellArgs` in `mkHaskellProject` in ./project.nix for more details.

{
  repoRoot,
  inputs,
  pkgs,
  lib,
  system,
}:

# Each flake variant defined in your project.nix project will yield a separate
# shell. If no flake variants are defined, then cabalProject is the original project.
cabalProject: {
  name = "run-script-evaluations";

  packages = [
    pkgs.figlet
  ];

  scripts = {

    evaluate = {
      description = "Evaluate Plutus Scripts from mainnet";
      group = "general";
      exec = ''
        cabal run exe:run-script-evaluations -- --start-block=0 --database-conn-str "$DB_CONN_STRING"
      '';
    };
  };

  shellHook = ''
    figlet "Plutus Script Evaluation: runner"
  '';

  preCommit = {
    cabal-fmt.enable = true;
    stylish-haskell.enable = false;
    fourmolu.enable = true;
    hlint.enable = true;
    editorconfig-checker.enable = false;
    nixpkgs-fmt.enable = false;
  };
}
