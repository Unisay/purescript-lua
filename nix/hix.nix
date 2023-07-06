{ pkgs, easy-ps, ... }: {
  name = "purescript-lua";
  compiler-nix-name = "ghc928";
  crossPlatforms = p:
    pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([ p.mingwW64 ]
      ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ p.musl64 ]);

  shell = {
    tools = let index-state = "2023-07-04T00:00:00Z";
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
      purescript
      spago
      treefmt
    ];
  };
}
