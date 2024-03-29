let upstream =
      https://github.com/Unisay/purescript-lua-package-sets/releases/download/psc-0.15.15-20240321/packages.dhall
        sha256:5b0a1f05293f3dfc6de9e2d87c5afab1cac35e2d9b38693f032f3e9a86a1090d

in  upstream
  with effect =
    { dependencies = [ "prelude" ]
    , repo = "https://github.com/Unisay/purescript-lua-effect.git"
    , version = "v4.1.0"
    }
  with prelude =
    { dependencies = [] : List Text
    , repo = "https://github.com/Unisay/purescript-lua-prelude.git"
    , version = "v7.1.0"
    }
