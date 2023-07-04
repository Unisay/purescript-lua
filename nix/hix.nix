{ pkgs, easy-ps, ... }: {
  name = "purescript-lua";
  compiler-nix-name = "ghc928";
  # crossPlatforms = p:
  #   pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([ p.mingwW64 ]
  #     ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ p.musl64 ]);

  shell = {
    tools =
      let index-state = "2023-06-29T00:00:00Z";
      in {
        cabal = {
          inherit index-state;
          version = "latest";
        };
        cabal-fmt = {
          inherit index-state;
          version = "latest";
        };
        hlint = {
          inherit index-state;
          version = "latest";
        };
        haskell-language-server = {
          inherit index-state;
          version = "latest";
        };
      };
    buildInputs = with pkgs; [
      haskellPackages.fourmolu_0_12_0_0
      lua53Packages.lua
      lua53Packages.luacheck
      nixpkgs-fmt
      purescript
      spago
      treefmt
    ];
  };
}
