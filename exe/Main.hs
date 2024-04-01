module Main where

import Cli (Args (luaOutputFile), ExtraOutput (..))
import Cli qualified
import Control.Monad.Oops qualified as Oops
import Data.Tagged (Tagged (..))
import Language.PureScript.Backend (CompilationResult (..))
import Language.PureScript.Backend qualified as Backend
import Language.PureScript.Backend.IR qualified as IR
import Language.PureScript.Backend.Lua qualified as Lua
import Language.PureScript.Backend.Lua.Printer qualified as Printer
import Language.PureScript.CoreFn.Reader qualified as CoreFn
import Language.PureScript.Names (runIdent, runModuleName)
import Main.Utf8 qualified as Utf8
import Path (Abs, Dir, Path, SomeBase (..), replaceExtension, toFilePath)
import Path.IO qualified as Path
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderIO)
import Text.Pretty.Simple (pHPrint)

main ∷ IO ()
main = Utf8.withUtf8 do
  Cli.Args
    { foreignPath
    , luaOutputFile
    , outputIR
    , outputLuaAst
    , psOutputPath
    , appOrModule
    } ←
    Cli.parseArguments

  foreignDir ∷ Tagged "foreign" (Path Abs Dir) ←
    Tagged
      <$> case unTagged foreignPath of
        Path.Abs a → pure a
        Path.Rel r → Path.makeAbsolute r

  luaOutput ←
    case unTagged luaOutputFile of
      Path.Abs a → pure a
      Path.Rel r → Path.makeAbsolute r

  let extraOutputs = catMaybes [outputLuaAst, outputIR]

  CompilationResult {lua, ir} ← do
    putTextLn "PS Lua: compiling ..."
    Backend.compileModules psOutputPath foreignDir appOrModule
      & handleModuleNotFoundError
      & handleModuleDecodingError
      & handleCoreFnError
      & handleLuaError
      & Oops.runOops

  let outputFile = toFilePath luaOutput
  withFile outputFile WriteMode \h →
    renderIO h . layoutPretty defaultLayoutOptions $
      Printer.printLuaChunk lua

  when (OutputIR `elem` extraOutputs) do
    irOutputFile ← toFilePath <$> replaceExtension ".ir" luaOutput
    withFile irOutputFile WriteMode (`pHPrint` ir)
    putTextLn $ "Wrote IR to " <> toText irOutputFile

  when (OutputLuaAst `elem` extraOutputs) do
    luaAstOutputFile ← toFilePath <$> replaceExtension ".lua-ast" luaOutput
    withFile luaAstOutputFile WriteMode (`pHPrint` lua)
    putTextLn $ "Wrote Lua AST to " <> toText luaAstOutputFile

  putTextLn $ "Wrote linked modules to " <> toText outputFile

--------------------------------------------------------------------------------
-- Error handlers --------------------------------------------------------------

handleModuleNotFoundError
  ∷ ExceptT (Oops.Variant (CoreFn.ModuleNotFound ': e)) IO a
  → ExceptT (Oops.Variant e) IO a
handleModuleNotFoundError = Oops.catch \(CoreFn.ModuleNotFound p) →
  die . toString . unlines $
    [ "Can't find CoreFn module file: " <> toText (toFilePath p)
    , "Please make sure you did run purs with the `-g corefn` arg."
    ]

handleModuleDecodingError
  ∷ ExceptT (Oops.Variant (CoreFn.ModuleDecodingErr ': e)) IO a
  → ExceptT (Oops.Variant e) IO a
handleModuleDecodingError = Oops.catch \(CoreFn.ModuleDecodingErr p e) →
  die . toString . unlines $
    [ "Can't parse CoreFn module file: " <> toText (toFilePath p)
    , toText e
    ]

handleCoreFnError
  ∷ ExceptT (Oops.Variant (IR.CoreFnError ': e)) IO a
  → ExceptT (Oops.Variant e) IO a
handleCoreFnError =
  Oops.catch \(e ∷ IR.CoreFnError) →
    die $ "CoreFn contains an unexpected value " <> show e

handleLuaError
  ∷ ExceptT (Oops.Variant (Lua.Error ': e)) IO a
  → ExceptT (Oops.Variant e) IO a
handleLuaError =
  Oops.catch \case
    Lua.UnexpectedRefBound modname expr →
      die . toString . unwords $
        [ "Unexpected bound reference:"
        , show expr
        , "in module"
        , runModuleName modname
        ]
    Lua.LinkerErrorForeign e →
      die $ "Linker error:\n" <> show e
    Lua.AppEntryPointNotFound modname ident →
      die . toString $
        "App entry point not found: "
          <> runModuleName modname
          <> "."
          <> runIdent ident
