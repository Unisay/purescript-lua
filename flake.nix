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
        hlib = pkgs.haskell.lib;
        overlays = [
          haskellNix.overlay
          (final: prev: {
            psluaProject = final.haskell-nix.project' {
              src = ./.;
              evalSystem = "x86_64-linux";
              modules = let prof = false;
              in [{
                doHaddock = false;
                doHoogle = false;
                enableProfiling = prof;
                enableLibraryProfiling = prof;
              }];

              name = "purescript-lua";
              compiler-nix-name = "ghc928";
              crossPlatforms = p:
                pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64
                ([ p.mingwW64 ]
                  ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux
                  [ p.musl64 ]);

              shell = {
                tools = let index-state = "2023-11-05T00:00:00Z";
                in {
                  cabal = {
                    inherit index-state;
                    version = "latest";
                  };
                  cabal-fmt = {
                    inherit index-state;
                    version = "latest";
                  };
                  fourmolu = {
                    inherit index-state;
                    version = "0.13.0.0";
                  };
                  hlint = {
                    inherit index-state;
                    version = "latest";
                  };
                  haskell-language-server = {
                    inherit index-state;
                    version = "latest";
                  };
                  nixfmt = {
                    inherit index-state;
                    version = "latest";
                  };
                };
                buildInputs = with pkgs; [
                  cachix
                  lua53Packages.lua
                  lua53Packages.luacheck
                  easy-ps.purs-0_15_10
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
        packages.static =
          flake.ciJobs.x86_64-unknown-linux-musl.packages."pslua:exe:pslua";
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
