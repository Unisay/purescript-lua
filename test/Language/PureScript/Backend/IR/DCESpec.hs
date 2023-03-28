module Language.PureScript.Backend.IR.DCESpec where

import Data.List.NonEmpty qualified as NE
import Hedgehog (annotate, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.PureScript.Backend.IR
import Language.PureScript.Backend.IR.DCE
  ( Strategy (..)
  , dceExpr
  , eliminateDeadCode
  )
import Language.PureScript.Backend.IR.Gen qualified as Gen
import Shower (shower)
import Test.Hspec (Spec, describe)
import Test.Hspec.Hedgehog.Extended (test)

spec :: Spec
spec = describe "IR Dead Code Elimination" do
  test "test not eliminate a module with an exported entry point" do
    let strategy = EntryPoints $ NE.singleton mainEntryPoint
    [entryPointModule] === eliminateDeadCode strategy [entryPointModule]

  test "eliminates unused non-exported binding" do
    let entryModule =
          entryPointModule
            { moduleBindings =
                binding_ "unused" : moduleBindings entryPointModule
            }
        strategy = EntryPoints $ NE.singleton mainEntryPoint
    [entryModule {moduleBindings = [binding_ "main"]}]
      === eliminateDeadCode strategy [entryModule]

  test "doesn't eliminate binding used from other module" do
    b@(Standalone (name, _expr)) <- forAll Gen.standaloneBinding
    modname <- forAll Gen.moduleName
    let
      entryModule =
        entryPointModule
          { moduleImports = [modname]
          , moduleBindings =
              [binding "main" $ refFreeImported modname name]
          }
      otherModule =
        emptyModule
          { moduleName = modname
          , moduleBindings = [b]
          , moduleExports = [name]
          }
      strategy = EntryPoints $ NE.singleton mainEntryPoint

    [entryModule, otherModule]
      === eliminateDeadCode strategy [entryModule, otherModule]

  test "eliminates binding not used from other module" do
    modname <- forAll Gen.moduleName
    let entryModule =
          entryPointModule
            { moduleImports = [modname]
            , moduleBindings =
                [binding "main" $ refFreeImported modname (Name "bar")]
            }
        otherModule =
          emptyModule
            { moduleName = modname
            , moduleBindings = [binding_ "foo"]
            , moduleExports = [Name "foo"]
            }
        strategy = EntryPoints $ NE.singleton mainEntryPoint
    [entryModule]
      === eliminateDeadCode strategy [entryModule, otherModule]

  test "detects named argument unused by an abs-bindings" do
    body <- forAll Gen.literalNonRecursiveExp
    name <- forAll Gen.name
    dceExpr (abstraction (ArgNamed name) body) === abstraction ArgUnused body

  test "detects anonymous argument unused by an abs-bindings" do
    body <- forAll Gen.literalNonRecursiveExp
    dceExpr (wrapExpF $ Abs $ AbsBinding ArgAnonymous (LocallyNameless body))
      === abstraction ArgUnused body

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
    let bindC = Standalone (c, refFreeLocal b)
        expr =
          lets
            (binding_ "unusedOuter" :| [bindA, bindB])
            ( lets
                (bindC :| [binding_ "unusedInner"])
                (application (refFreeLocal c) (refFreeLocal a))
            )
        expected =
          lets
            (bindA :| [bindB])
            ( lets
                (pure bindC)
                (application (refFreeLocal c) (refFreeLocal a))
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
            ( RecursiveGroup ((a, refFreeLocal b) :| [(b, refFreeLocal a)])
                :| []
            )
            (refFreeLocal c)
        expected = refFreeLocal c
    annotate $ shower expr
    expected === dceExpr expr

--------------------------------------------------------------------------------
-- Fixture ---------------------------------------------------------------------

mainEntryPoint :: (ModuleName, NonEmpty Name)
mainEntryPoint = (ModuleName "Main", NE.singleton (Name "main"))

emptyModule :: Module
emptyModule =
  Module
    { moduleName = ModuleName "m1"
    , moduleBindings = []
    , moduleImports = moduleImports1
    , moduleExports = moduleExports1
    , moduleReExports = moduleReExports1
    , moduleForeigns = moduleForeigns1
    , modulePath = "m1"
    , dataTypes = mempty
    }

entryPointModule :: Module
entryPointModule =
  emptyModule
    { moduleName = fst mainEntryPoint
    , moduleExports = toList $ snd mainEntryPoint
    , moduleBindings = [binding_ "main"]
    }

binding :: Text -> b -> Grouping (Name, b)
binding n e = Standalone (Name n, e)

binding_ :: Text -> Binding
binding_ n = binding n (exception n)

moduleImports1 :: [ModuleName]
moduleImports1 = []

moduleExports1 :: [Name]
moduleExports1 = []

moduleReExports1 :: Map ModuleName [Name]
moduleReExports1 = mempty

moduleForeigns1 :: [Name]
moduleForeigns1 = []
