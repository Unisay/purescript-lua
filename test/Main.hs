module Main where

import Language.PureScript.Backend.IR.DCESpec qualified as DCE
import Language.PureScript.Backend.IR.LocallyNamelessSpec qualified as LocallyNameless
import Language.PureScript.Backend.IR.OptimizerSpec qualified as IROptimizer
import Language.PureScript.Backend.IRSpec qualified as IR
import Language.PureScript.Backend.Lua.GoldenSpec qualified as Golden
import Language.PureScript.Backend.Lua.OptimizerSpec qualified as LuaOptimizer
import Language.PureScript.Backend.Lua.PrinterSpec qualified as Printer
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
  IR.spec
  LocallyNameless.spec
  Golden.spec
  DCE.spec
  IROptimizer.spec
  LuaOptimizer.spec
  Printer.spec
