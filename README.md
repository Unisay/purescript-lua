# Purescript Backend for Lua

Status: (2023-07-05) the project is in the "_ready to be experimented with_" state (read: it likely contains bugs but is already usable).

## Features

- [x] Lua code bundling: emits either a Lua module or "App".
- [x] FFI with Lua.
- [x] Dead Code Elimination (DCE).
- [x] Code inlining.
- [x] [Package Set](https://github.com/Unisay/purescript-lua-package-sets) for PureScript/Lua libs.
- [x] All core libs added to the package set.

## Quick Start

For the moment the best way to start is to use `nix` to intall `pslua`.

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
    --lua-output-file dist/acme.lua \
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

### Using nix with flakes

```
nix profile install 'github:Unisay/purescript-lua'
```

will make `pslua` executable available for use.
