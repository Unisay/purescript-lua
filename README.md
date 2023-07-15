# Purescript Backend for Lua

[![Purescript Lua CI](https://github.com/Unisay/purescript-lua/actions/workflows/ci.yaml/badge.svg)](https://github.com/Unisay/purescript-lua/actions/workflows/ci.yaml)

Status: (2023-07-05) the project is in the "_ready to be experimented with_" state (read: it likely contains bugs but is already usable).

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
