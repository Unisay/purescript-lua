# Purescript Backend for Lua

Status: experimental (expect bugs)

## Features

- [x] Lua code bundling: emits either a Lua module or "App".
- [X] FFI with Lua.
- [x] Dead Code Elimination (DCE).
- [ ] Code inlining.
- [x] [Package Set](https://github.com/Unisay/purescript-lua-package-sets) for PureScript/Lua libs.

## Quick Start

For the moment the best way to start is to use `nix` to intall `pslua`. 
Here is an [example](https://github.com/Unisay/purescript-lua-example) project.
