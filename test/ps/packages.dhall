let upstream-ps =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20240419/packages.dhall
        sha256:50c4ee579bf2c38671ac97df821c2cc4221fb3f6ad79c807bb6e4597ab6d1e95

let upstream-lua =
      https://github.com/Unisay/purescript-lua-package-sets/releases/download/psc-0.15.15-20240416/packages.dhall
        sha256:e68b7752ca4dee0f0578a2e40159caf6d1290a711777931b20d10d807823b52d

in  upstream-ps // upstream-lua

