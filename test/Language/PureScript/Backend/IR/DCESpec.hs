module Language.PureScript.Backend.IR.DCESpec where

import Hedgehog (annotate, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.PureScript.Backend.IR
import Language.PureScript.Backend.IR.DCE (EntryPoint (..), dceExpr, eliminateDeadCode)
import Language.PureScript.Backend.IR.Gen qualified as Gen
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Shower (shower)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog.Extended (hedgehog, test)

spec :: Spec
spec = describe "IR Dead Code Elimination" do
  test "doesn't eliminate an exported entry point" do
    entryPointModule === eliminateDeadCode mainEntryPoint entryPointModule

  test "eliminates unused non-exported binding" do
    entryPointModule
      === eliminateDeadCode
        mainEntryPoint
        entryPointModule
          { uberModuleBindings =
              topBinding_ "unused" : uberModuleBindings entryPointModule
          }

  test "detects named argument unused by an abs-bindings" do
    body <- forAll Gen.exp
    name <- forAll Gen.name
    dceExpr (abstraction (ParamNamed name) body) === abstraction ParamUnused body

  it "doesn't eliminate named argument used by an abs-bindings" $ hedgehog do
    name <- forAll Gen.name
    let f = abstraction (ParamNamed name) (refLocal0 name)
    dceExpr f === f

  test "eliminates unused non-recursive let-bindings" do
    {-
        let unusedOuter = exception "unusedOuter"
            a = 0
            b = 0
         in let unusedInner = exception "unusedInner"
                c = b{1,2}
             in c{0,1} a{1,1}

    should be transformed to:

        let a = 0
            b = 0
         in let c = b{1,1}
             in c{0,0} a{1,0}
    -}
    [a, b, c] <- forAll $ toList <$> Gen.set (Range.singleton 3) Gen.name
    bindA <- Standalone . (a,) <$> forAll Gen.literalNonRecursiveExp
    bindB <- Standalone . (b,) <$> forAll Gen.literalNonRecursiveExp
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
    expected === dceExpr expr

  test "eliminates unused recursive let-bindings" do
    {-
    let a = b
        b = a
     in c
    -}
    [a, b, c] <- forAll $ toList <$> Gen.set (Range.singleton 3) Gen.name
    let expr =
          lets
            ( RecursiveGroup ((a, refLocal0 b) :| [(b, refLocal0 a)])
                :| []
            )
            (refLocal0 c)
        expected = refLocal0 c
    annotate $ shower expr
    expected === dceExpr expr

--------------------------------------------------------------------------------
-- Fixture ---------------------------------------------------------------------

mainEntryPoint :: EntryPoint
mainEntryPoint = EntryPoint (ModuleName "Main") [Name "main"]

emptyModule :: UberModule
emptyModule =
  UberModule
    { uberModuleBindings = []
    , uberModuleForeigns = []
    , uberModuleExports = []
    }

entryPointModule :: UberModule
entryPointModule = emptyModule {uberModuleBindings = [topBinding_ "main"]}

binding_ :: Text -> Grouping (Name, Exp)
binding_ n = Standalone (Name n, exception n)

topBinding_ :: Text -> Grouping (QName, Exp)
topBinding_ n = Standalone (QName (ModuleName "Main") (Name n), exception n)
