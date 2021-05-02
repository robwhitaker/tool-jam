{
  description = "Daemon that handles changes in your ~/tools directory";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskell.packages.ghc8104.override {
          overrides = final: prev:
            let
              hsOverride = pkg: final.callPackage (./nix-overrides + "/${pkg}.nix") {};
            in {
              relude = final.relude_1_0_0_1;
              taskd = final.callPackage ./. {};
            };
        };
        projectGhc = haskellPackages.ghcWithHoogle (_:
          haskellPackages.taskd.getBuildInputs.haskellBuildInputs
        );
      in
      rec {
        packages = flake-utils.lib.flattenTree {
          taskd = haskellPackages.taskd;
        };
        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            projectGhc

            cabal2nix
            cabal-install

            haskell-language-server
          ];
        };
        defaultPackage = packages.taskd;
      }
    );
}
