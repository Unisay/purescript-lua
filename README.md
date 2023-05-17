# Purescript Backend for Lua

Status: (2023-05-17) the project is in the "*almost ready to be usable*" state (read: it isn't working as you'd expect it to work), I am finilizing core libs and final optimisations in the backend.

## Features

- [x] Lua code bundling: emits either a Lua module or "App".
- [X] FFI with Lua.
- [x] Dead Code Elimination (DCE).
- [ ] Code inlining.
- [x] [Package Set](https://github.com/Unisay/purescript-lua-package-sets) for PureScript/Lua libs.
- [ ] All core libs added to the package set.

## Quick Start

For the moment the best way to start is to use `nix` to intall `pslua`. 
Here is an [example](https://github.com/Unisay/purescript-lua-example) project.
