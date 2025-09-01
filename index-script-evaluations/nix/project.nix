{ inputs, pkgs, lib }:

let
  cabalProject = pkgs.haskell-nix.cabalProject' (
    
    { config, pkgs, ... }:

    {
      name = "index-script-evaluations";

      compiler-nix-name = lib.mkDefault "ghc966";

      src = lib.cleanSource ../.;

      flake.variants = { ghc966 = {}; };

      inputMap = { "https://chap.intersectmbo.org/" = inputs.CHaP; };

      modules = [{ packages = {}; }];
    }
  );

in

cabalProject
