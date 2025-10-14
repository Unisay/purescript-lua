# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is `pslua` - a PureScript to Lua compiler backend. It takes PureScript CoreFn (the PureScript compiler's intermediate representation) and compiles it to Lua. The project supports dead code elimination (DCE), code inlining, FFI with Lua, and can emit either Lua modules or standalone applications.

## Build System & Commands

The project uses **Nix with flakes** for reproducible builds and **Cabal** for Haskell development.

### Development Environment

```bash
# Enter development shell (provides all tools)
nix develop

# Or use direnv (if configured)
direnv allow
```

### Building

```bash
# Build the project
cabal build

# Build specific component
cabal build exe:pslua
cabal build lib:pslua
```

### Testing

```bash
# Run all tests with detailed output
cabal test all --test-show-details=direct

# Run specific test suite
cabal test spec

# Run tests in watch mode (requires ghcid)
ghcid --command="cabal repl test:spec" --test=":main"
```

The test suite includes:
- **Unit tests**: Property-based testing with Hedgehog
- **Golden tests**: Compiles PureScript test modules from `test/ps/golden/Golden/*/Test.purs` to Lua and compares against golden files
- **Evaluation tests**: Runs generated Lua code and verifies output
- **Luacheck tests**: Validates generated Lua code syntax

### Testing PureScript Code

Golden tests require compiling PureScript sources first:

```bash
# Compile PureScript test sources (from test/ps directory)
cd test/ps
spago build -u '-g corefn'
cd ../..
```

### Resetting Golden Files

```bash
# Remove all golden files and regenerate them
./scripts/golden_reset
```

This finds all files named `golden.*` in `test/ps/output` and deletes them, then runs `cabal test` to regenerate them.

### Code Formatting & Linting

```bash
# Format Haskell code with Fourmolu
fourmolu -i lib/ exe/ test/

# Or use treefmt to format all files
treefmt

# Run HLint
hlint lib/ exe/ test/

# Check Lua files
luacheck --quiet --std min test/ps/output/
```

### Running the Compiler

```bash
# Build and run
cabal run pslua -- --help

# Or after building
./dist-newstyle/build/x86_64-linux/ghc-9.8.1/pslua-0.1.0.0/x/pslua/build/pslua/pslua --help

# Or via nix
nix run . -- --help
```

Typical usage:
```bash
pslua \
  --foreign-path ./foreign \
  --ps-output ./output \
  --lua-output-file ./dist/Main.lua \
  --entry Main.main
```

## Code Architecture

### Compilation Pipeline

The compilation happens in several distinct phases:

```
PureScript Source → CoreFn → IR → Lua → Optimized Lua
```

1. **CoreFn Reading** (`Language.PureScript.CoreFn.*`)
   - Reads PureScript compiler's CoreFn JSON output
   - Parses module structure, imports, and expressions

2. **IR Translation** (`Language.PureScript.Backend.IR.*`)
   - Converts CoreFn to an intermediate representation (IR)
   - IR is simpler than CoreFn but still high-level
   - Handles data declarations, bindings, and module structure

3. **IR Optimization** (`Language.PureScript.Backend.IR.Optimizer`)
   - Performs optimizations on IR:
     - Eta reduction/expansion
     - Beta reduction
     - Constant folding
     - Case-of-case transformation
   - **Inliner** (`IR.Inliner`): Marks expressions for inlining
   - **Dead Code Elimination** (`IR.DCE`): Removes unused bindings

4. **Linking** (`Language.PureScript.Backend.IR.Linker`)
   - Creates an "UberModule" containing all reachable code
   - Supports two modes:
     - `LinkAsModule`: Creates a Lua module (returns a table)
     - `LinkAsApplication`: Creates a runnable Lua script (calls entry point)

5. **Lua Code Generation** (`Language.PureScript.Backend.Lua`)
   - Converts optimized IR to Lua AST
   - Handles foreign imports via FFI
   - Manages name mangling and scope

6. **Lua Optimization** (`Language.PureScript.Backend.Lua.Optimizer`)
   - Optimizes generated Lua code

7. **Lua Printing** (`Language.PureScript.Backend.Lua.Printer`)
   - Pretty-prints Lua AST to text

### Key Module Structure

**CoreFn Layer** (`Language.PureScript.CoreFn.*`):
- `CoreFn.Reader`: Reads CoreFn JSON from disk
- `CoreFn.FromJSON`: JSON deserialization
- `CoreFn.Expr`: CoreFn expression types
- `CoreFn.Traversals`: Traversal utilities

**IR Layer** (`Language.PureScript.Backend.IR.*`):
- `IR.Types`: Core IR data types (`RawExp`, `Module`, `Binding`)
- `IR.Names`: Name types (`Qualified`, `ModuleName`, etc.)
- `IR.Linker`: Creates UberModule from multiple modules
- `IR.Optimizer`: IR-level optimizations
- `IR.DCE`: Dead code elimination
- `IR.Inliner`: Inlining annotations and logic

**Lua Backend** (`Language.PureScript.Backend.Lua.*`):
- `Lua.Types`: Lua AST types (`Chunk`, `Statement`, `Exp`)
- `Lua.Name`: Safe Lua identifier generation
- `Lua.Printer`: Pretty-printing Lua code
- `Lua.Optimizer`: Lua-level optimizations
- `Lua.DCE`: Lua-specific DCE
- `Lua.Linker.Foreign`: FFI support for Lua foreign modules
- `Lua.Fixture`: Runtime support code injected into output

**Main Entry** (`Language.PureScript.Backend`):
- `compileModules`: Top-level compilation function orchestrating the pipeline

### Important Concepts

**De Bruijn Indices**: The IR uses De Bruijn indices for variable references. A `Ref` contains:
- A qualified name
- An index indicating which binding of that name to reference

**Groupings**: Bindings are wrapped in `Grouping`:
- `Standalone`: Non-recursive binding
- `RecursiveGroup`: Mutually recursive bindings

**AppOrModule**: Determines compilation mode:
- `AsModule ModuleName`: Generate a Lua module
- `AsApplication ModuleName Ident`: Generate an executable that calls the entry point

**UberModule**: A flattened representation of all modules after linking, containing:
- All reachable bindings
- Module exports
- Foreign imports

## Code Style

### Haskell Style

This project follows specific Haskell style conventions:

- **Indentation**: 2 spaces (enforced by Fourmolu)
- **Line length**: Max 80 characters
- **Unicode**: Always use unicode syntax (`∷` instead of `::`, `→` instead of `->`)
- **Prelude**: Uses Relude (not base Prelude)
- **Imports**: Explicit qualified imports preferred
- **Extensions**: Many enabled by default (see `pslua.cabal` common stanza)

### Formatting Configuration

- **Fourmolu** (`fourmolu.yaml`):
  - 2-space indentation
  - 80-character column limit
  - Leading commas and function arrows
  - Unicode always
  - Multi-line Haddock style

- **HLint** (`.hlint.yaml`):
  - Configured with project-specific extensions
  - Run with `--color --cpp-simple -XQuasiQuotes -XImportQualifiedPost`

### Section Comments

Use section-style comments to organize code:

```haskell
--------------------------------------------------------------------------------
-- Section Title ---------------------------------------------------------------

code here...
```

Both lines should be exactly 80 characters. Helper functions go at the bottom after a "Helper Functions" or "Utility Functions" section.

## Testing Strategy

### Golden Tests

Golden tests are the primary integration testing mechanism:

1. PureScript test files in `test/ps/golden/Golden/*/Test.purs`
2. Compiled to CoreFn with `spago build -u '-g corefn'`
3. Test suite reads CoreFn, compiles to IR, generates Lua
4. Compares against golden files:
   - `golden.ir` - Intermediate representation
   - `golden.lua` - Generated Lua code
   - `eval/golden.txt` - Execution output (if module has a `main` function)

To add a new golden test:
1. Create `test/ps/golden/Golden/NewTest/Test.purs`
2. Run `cabal test` - it will fail and create `actual.*` files
3. Review the actual files
4. Rename `actual.*` to `golden.*` if correct
5. Commit the golden files

### Property-Based Tests

The project uses Hedgehog for property-based testing:
- Generators in `test/Language/PureScript/Backend/IR/Gen.hs`
- Tests in `test/Language/PureScript/Backend/IR/Spec.hs` and similar

## Development Workflow

1. Make code changes in `lib/` or `exe/`
2. Format code: `fourmolu -i lib/ exe/ test/`
3. Run HLint: `hlint lib/ exe/ test/`
4. Run tests: `cabal test all --test-show-details=direct`
5. If golden tests fail, inspect `actual.*` files in `test/ps/output/`
6. Update golden files if changes are correct

## Debugging Tips

- The IR and Lua types have `Show` instances - use `pShowOpt` for pretty debug output
- Golden test failures show diffs between expected and actual
- Use `actual.*` files alongside `golden.*` files to debug compilation issues
- Check `test/ps/output/Golden.*/` directories for generated IR and Lua
- Lua evaluation errors are captured in the test output
