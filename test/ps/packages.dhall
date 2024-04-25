let upstream-ps =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20240419/packages.dhall
        sha256:50c4ee579bf2c38671ac97df821c2cc4221fb3f6ad79c807bb6e4597ab6d1e95

let upstream-lua =
      https://github.com/Unisay/purescript-lua-package-sets/releases/download/psc-0.15.15-20240425/packages.dhall
        sha256:3721bc8a2f6681e16fb505b8b0256650d9fd47977755fde9947852343888f14b

in  upstream-ps // upstream-lua
