# Purescript Backend for Lua

Status: (2023-07-05) the project is in the "*ready to be experimented with*" state (read: it likely contains bugs but is already usable).

## Features

- [x] Lua code bundling: emits either a Lua module or "App".
- [X] FFI with Lua.
- [x] Dead Code Elimination (DCE).
- [x] Code inlining.
- [x] [Package Set](https://github.com/Unisay/purescript-lua-package-sets) for PureScript/Lua libs.
- [x] All core libs added to the package set.

## Quick Start

For the moment the best way to start is to use `nix` to intall `pslua`. 
Here is an [example](https://github.com/Unisay/purescript-lua-example) project.
