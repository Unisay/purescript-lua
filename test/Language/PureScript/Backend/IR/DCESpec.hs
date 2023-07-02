module Language.PureScript.Backend.IR.DCESpec where

import Data.Map qualified as Map
import Hedgehog (Gen, annotate, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.PureScript.Backend.IR.DCE
  ( EntryPoint (..)
  , eliminateDeadCode
  )
import Language.PureScript.Backend.IR.Gen qualified as Gen
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Types
  ( Exp
  , Grouping (..)
  , Name (Name)
  , Parameter (ParamNamed, ParamUnused)
  , QName (QName)
  , Qualified (..)
  , abstraction
  , application
  , countFreeRefs
  , exception
  , lets
  , refImported
  , refLocal0
  )
import Language.PureScript.Names (ModuleName, moduleNameFromString)
import Shower (shower)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog.Extended (hedgehog, test)

spec ∷ Spec
spec = describe "IR Dead Code Elimination" do
  let singletonModule ∷ Gen UberModule
      singletonModule = do
        name ← Gen.name
        moduleName ← Gen.moduleName
        expr ← Gen.nonRecursiveExp
        pure
          emptyModule
            { uberModuleBindings = [Standalone (QName moduleName name, expr)]
            , uberModuleExports = [(name, refImported moduleName name 0)]
            }

  test "doesn't eliminate an exported entry point" do
    optimalModule ← forAll singletonModule
    optimalModule === eliminateDeadCode optimalModule

  test "eliminates unused non-exported binding" do
    expected@UberModule {uberModuleBindings} ← forAll singletonModule
    let unoptimized =
          expected
            { uberModuleBindings = topBinding_ "unused" : uberModuleBindings
            }
    annotate $ shower unoptimized
    expected === eliminateDeadCode unoptimized

  test "detects named parameter unused by an abs-bindings" do
    body ← forAll Gen.exp
    let names = [name | Local name ← Map.keys (countFreeRefs body)]
    name ← forAll $ mfilter (`notElem` names) Gen.name
    dceExpression (abstraction (ParamNamed name) body)
      === abstraction ParamUnused body

  it "doesn't eliminate named parameter used by an abs-bindings" $ hedgehog do
    name ← forAll Gen.name
    let f = abstraction (ParamNamed name) (refLocal0 name)
    dceExpression f === f

  test "eliminates unused non-recursive let-bindings" do
    {-
        let unusedOuter = exception "unusedOuter"
            a = 0
            b = 0
         in let unusedInner = exception "unusedInner"
                c = b
             in c a

    should be transformed to:

        let a = 0
            b = 0
         in let c = b
             in c a
    -}
    [a, b, c] ← forAll $ toList <$> Gen.set (Range.singleton 3) Gen.name
    bindA ← Standalone . (a,) <$> forAll Gen.literalNonRecursiveExp
    bindB ← Standalone . (b,) <$> forAll Gen.literalNonRecursiveExp
    let bindC = Standalone (c, refLocal0 b)
        expr =
          lets
            (binding_ "unusedOuter" :| [bindA, bindB])
            ( lets
                (bindC :| [binding_ "unusedInner"])
                (application (refLocal0 c) (refLocal0 a))
            )
        expected =
          lets
            (bindA :| [bindB])
            ( lets
                (pure bindC)
                (application (refLocal0 c) (refLocal0 a))
            )
    annotate $ shower expr
    expected === dceExpression expr

  test "eliminates unused recursive let-bindings" do
    {-
    let a = b
        b = a
     in c
    -}
    [a, b, c] ← forAll $ toList <$> Gen.set (Range.singleton 3) Gen.name
    let expr =
          lets
            (RecursiveGroup ((a, refLocal0 b) :| [(b, refLocal0 a)]) :| [])
            (refLocal0 c)
        expected = refLocal0 c
    annotate $ shower expr
    expected === dceExpression expr

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

dceExpression ∷ HasCallStack ⇒ Exp → Exp
dceExpression e =
  let res =
        uberModuleExports $
          eliminateDeadCode
            emptyModule
              { uberModuleExports = [(Name "main", e)]
              }
   in case res of
        [(Name "main", e')] → e'
        _ → error $ "dceExpression: unexpected result: " <> show res

--------------------------------------------------------------------------------
-- Fixture ---------------------------------------------------------------------

mainModuleName ∷ ModuleName
mainModuleName = moduleNameFromString "Main"

mainEntryPoint ∷ EntryPoint
mainEntryPoint = EntryPoint mainModuleName [Name "main"]

emptyModule ∷ UberModule
emptyModule =
  UberModule
    { uberModuleBindings = []
    , uberModuleForeigns = []
    , uberModuleExports = []
    }

binding_ ∷ Text → Grouping (Name, Exp)
binding_ n = Standalone (Name n, exception n)

topBinding_ ∷ Text → Grouping (QName, Exp)
topBinding_ n = Standalone (QName mainModuleName (Name n), exception n)
