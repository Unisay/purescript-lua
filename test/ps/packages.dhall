let upstream-ps =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20260605/packages.dhall
        sha256:e48c9b283ca89ec994453459fb74c4b5b5a9432349f83a2e104f39dd869a0f6e

let upstream-lua =
      https://github.com/Unisay/purescript-lua-package-sets/releases/download/psc-0.15.15-20260612/packages.dhall
        sha256:e031a7820831578424a5270b778f9f31a5aadee7a719a9fa95f8d58f406b308d

in  upstream-ps // upstream-lua
