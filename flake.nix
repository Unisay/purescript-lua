{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    easy-ps = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };

  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, easy-ps }:
    let supportedSystems = [ "x86_64-linux" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        hlib = pkgs.haskell.lib;
        overlays = [
          haskellNix.overlay
          (final: prev: {
            hixProject = final.haskell-nix.hix.project {
              src = ./.;
              evalSystem = "x86_64-linux";
              # index-state = "2023-06-28T00:00:00Z";
              modules = let prof = false;
              in [{
                doHaddock = false;
                doHoogle = false;
                enableProfiling = prof;
                enableLibraryProfiling = prof;
              }];
            };
          })
        ];
        flake = pkgs.hixProject.flake { };
      in flake // {
        legacyPackages = pkgs;
        packages.default = flake.packages."pslua:exe:pslua";
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
