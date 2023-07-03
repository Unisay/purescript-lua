module Main where

import Language.PureScript.Backend.IR.DCESpec qualified as IrDce
import Language.PureScript.Backend.IR.OptimizerSpec qualified as IROptimizer
import Language.PureScript.Backend.IR.TypesSpec qualified as Types
import Language.PureScript.Backend.IRSpec qualified as IR
import Language.PureScript.Backend.Lua.DeadCodeEliminatorSpec qualified as LuaDce
import Language.PureScript.Backend.Lua.GoldenSpec qualified as Golden
import Language.PureScript.Backend.Lua.OptimizerSpec qualified as LuaOptimizer
import Language.PureScript.Backend.Lua.PrinterSpec qualified as Printer
import Test.Hspec (hspec)

main ∷ IO ()
main = hspec do
  IR.spec
  Golden.spec
  IrDce.spec
  LuaDce.spec
  Types.spec
  IROptimizer.spec
  LuaOptimizer.spec
  Printer.spec
