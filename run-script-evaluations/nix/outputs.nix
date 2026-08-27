{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };

  utils = import ./utils.nix { inherit pkgs lib; };

  project = import ./project.nix { inherit inputs pkgs lib; };

  mkShell =
    ghc:
    import ./shell.nix {
      inherit
        inputs
        pkgs
        lib
        project
        utils
        ghc
        ;
    };

  devShells = rec {
    default = ghc966;
    ghc966 = mkShell "ghc966";
  };

  projectFlake = project.flake { };

  # Build targets for the executables, so `nix build` can verify the project.
  packages = projectFlake.packages;

  apps = projectFlake.apps;

  # nix/project.nix declares only the ghc966 variant, so that is the only
  # hydraJobs attribute that exists.
  defaultHydraJobs = {
    ghc966 = projectFlake.hydraJobs.ghc966;
    inherit packages;
    inherit devShells;
    required = utils.makeHydraRequiredJob hydraJobs;
  };

  hydraJobsPerSystem = {
    "x86_64-linux" = defaultHydraJobs;
    "x86_64-darwin" = defaultHydraJobs;
    "aarch64-linux" = defaultHydraJobs;
    "aarch64-darwin" = defaultHydraJobs;
  };

  hydraJobs = utils.flattenDerivationTree "-" hydraJobsPerSystem.${system};
in

{
  inherit apps;
  inherit packages;
  inherit devShells;
  inherit hydraJobs;
}
