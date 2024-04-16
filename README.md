# Purescript Backend for Lua

[![Purescript Lua CI](https://github.com/Unisay/purescript-lua/actions/workflows/ci.yaml/badge.svg)](https://github.com/Unisay/purescript-lua/actions/workflows/ci.yaml)

🔋 Status: (2024-04-16) the project is in the "_ready to be experimented with_" state (read: it likely contains bugs but is already usable). 

⚠️ Known problems: in larger projects produced Lua script can fail with the following error: 
```
function at line NNN has more than 60 upvalues
```
(I am working on the solution). 

💡 If you have an idea on how to use Purescript to Lua compilation please contribute it here:
https://github.com/Unisay/purescript-lua/discussions/categories/ideas

## Features

- [x] Lua code bundling: emits either a Lua module (a file that returns a table with functions) or an application (a file that executes itself).
- [x] FFI with Lua.
- [x] Dead Code Elimination (DCE).
- [x] Code inlining.
- [x] [Package Set](https://github.com/Unisay/purescript-lua-package-sets) for PureScript/Lua libs.
- [x] All core libs added to the package set.

## Quick Start

For the moment the best way to start is to use `nix` to intall `pslua`.

Consider configuring [Cachix](https://docs.cachix.org/installation) as a binary nix cache to avoid rebuilding a ton of dependencies:

```
cachix use purescript-lua
```

Here is an [example](https://github.com/Unisay/purescript-lua-example) project.

If you use [Spago](https://github.com/purescript/spago) to build your PureScript project, then you can configure `pslua` as a custom backend like this:

<details> <summary>spago.dhall</summary>

Assuming that `pslua` executable is already available on your PATH

```dhall
{ name = "acme-project"
, dependencies = [ "effect", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend =
    ''
    pslua \
    --foreign-path . \
    --ps-output output \
    --lua-output-file dist/Acme_Main.lua \
    --entry Acme.Main
    ''
}
```

</details>

### Using nix with flakes

```
nix run 'github:Unisay/purescript-lua' -- --help
```

## Installation

If you're on a x86 64bit Linux system then you can download a pre-built executable from the [releases](https://github.com/Unisay/purescript-lua/releases) page:

```
wget -c https://github.com/Unisay/purescript-lua/releases/download/0.1.1-alpha/pslua-linux_x86_64.tar.gz -O - | tar -xz
```

alternatively,

### Using nix with flakes

```
nix profile install 'github:Unisay/purescript-lua'
```

will make `pslua` executable available for use.

### Windows

Nix build won't work on Windows so you'd first need to  install
`cabal` and `ghc-9.4.8` (One way of installing those is [GHCUp](https://www.haskell.org/ghcup/)).

Once the pre-requisites are available on your PATH
you run

```
cabal install exe:pslua

.... elided ....

Installing   commutative-semigroups-0.1.0.1 (lib)
Installing   primes-0.2.1.0 (all, legacy fallback)
Installing   base16-bytestring-1.0.2.0 (lib)
Installing   quiet-0.2 (lib)
Completed    newtype-0.2.2.0 (lib)

.... elided ....

Starting     pslua-0.1.0.0 (exe:pslua)
Building     pslua-0.1.0.0 (exe:pslua)
Installing   pslua-0.1.0.0 (exe:pslua)
Completed    pslua-0.1.0.0 (exe:pslua)
Copying 'pslua.exe' to 'C:\cabal\bin\pslua.exe'
```

This will build and install executable `pslua.exe`

```
C:\cabal\bin\pslua --help
pslua - a PureScript backend for Lua

Usage: pslua.exe [--foreign-path FOREIGN-PATH] [--ps-output PS-PATH]
                 [--lua-output-file LUA-OUT-FILE] [-e|--entry ENTRY]

  Compile PureScript's CoreFn to Lua

Available options:
  --foreign-path FOREIGN-PATH
                           Path to a directory containing foreign files.
                           Default: foreign
  --ps-output PS-PATH      Path to purs output directory.
                           Default: output
  --lua-output-file LUA-OUT-FILE
                           Path to write compiled Lua file to.
                           Default: main.lua
  -e,--entry ENTRY         Where to start compilation.
                           Could be one of the following formats:
                           - Application format: <Module>.<binding>
                             Example: Acme.App.main
                           - Module format: <Module>
                             Example: Acme.Lib
                           Default: Main.main
  -h,--help                Show this help text
```
