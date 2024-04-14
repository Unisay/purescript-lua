module Main where

import Language.PureScript.Backend.IR.DCE.Spec qualified as IrDce
import Language.PureScript.Backend.IR.Inliner.Spec qualified as Inliner
import Language.PureScript.Backend.IR.Optimizer.Spec qualified as IROptimizer
import Language.PureScript.Backend.IR.Spec qualified as IR
import Language.PureScript.Backend.IR.Types.Spec qualified as Types
import Language.PureScript.Backend.Lua.DeadCodeEliminator.Spec qualified as LuaDce
import Language.PureScript.Backend.Lua.Golden.Spec qualified as Golden
import Language.PureScript.Backend.Lua.Linker.Foreign.Spec qualified as LuaLinkerForeign
import Language.PureScript.Backend.Lua.Optimizer.Spec qualified as LuaOptimizer
import Language.PureScript.Backend.Lua.Printer.Spec qualified as Printer
import Test.Hspec (hspec)

main ∷ IO ()
main = hspec do
  IR.spec
  Inliner.spec
  Golden.spec
  IrDce.spec
  LuaDce.spec
  Types.spec
  IROptimizer.spec
  LuaOptimizer.spec
  Printer.spec
  LuaLinkerForeign.spec
