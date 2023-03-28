{ pkgs, easy-ps, ... }: {
  name = "purescript-lua";
  compiler-nix-name = "ghc927";
  crossPlatforms = p:
    pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([ p.mingwW64 ]
      ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ p.musl64 ]);

  shell = {
    tools = let index-state = "2023-03-28T00:00:00Z";
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
    buildInputs =
      [ pkgs.purescript pkgs.lua53Packages.lua pkgs.lua53Packages.luacheck ];
  };
}
