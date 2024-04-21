{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, easy-purescript-nix }:
    let supportedSystems = [ "x86_64-linux" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        easy-ps = easy-purescript-nix.packages.${system};
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        overlays = [
          haskellNix.overlay
          (final: prev: {
            psluaProject = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc98";
              evalSystem = "x86_64-linux";
              modules = let prof = false;
              in [{
                doHaddock = false;
                doHoogle = false;
                enableProfiling = prof;
                enableLibraryProfiling = prof;
              }];

              name = "purescript-lua";

              shell = {
                tools = {
                  cabal = {};
                  fourmolu = {};
                  hlint = {};
                  haskell-language-server = {};
                };
                buildInputs = with pkgs; [
                  cachix
                  lua51Packages.lua
                  lua51Packages.luacheck
                  easy-ps.purs-0_15_15
                  easy-ps.spago
                  treefmt
                  upx
                  yamlfmt
                ];
              };

            };
          })
        ];
        flake = pkgs.psluaProject.flake { };
      in flake // {
        legacyPackages = pkgs;
        packages.default = flake.packages."pslua:exe:pslua";
        packages.static = flake.ciJobs.x86_64-unknown-linux-musl.packages."pslua:exe:pslua";
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters =
      [ "https://cache.iog.io" "https://purescript-lua.cachix.org" ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "purescript-lua.cachix.org-1:yLs4ei2HtnuPtzLekOrW3xdfm95+Etw15gwgyIGTayA="
    ];
    allow-import-from-derivation = "true";
  };
}
