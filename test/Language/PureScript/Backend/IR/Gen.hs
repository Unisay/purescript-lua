module Language.PureScript.Backend.IR.Gen where

import Data.Text qualified as Text
import Hedgehog (MonadGen)
import Hedgehog.Corpus qualified as Corpus
import Hedgehog.Gen.Extended qualified as Gen
import Hedgehog.Range qualified as Range
import Language.PureScript.Backend.IR.Types qualified as IR
import Prelude hiding (exp)

exp :: forall m. MonadGen m => m IR.Exp
exp =
  Gen.recursiveFrequency
    [(1, nonRecursiveExp)]
    [
      ( 7
      , Gen.subterm2 exp exp IR.application
      )
    ,
      ( 3
      , Gen.subterm3 exp exp exp IR.ifThenElse
      )
    ,
      ( 1
      , Gen.subtermM exp \e ->
          IR.arrayIndex e <$> Gen.integral (Range.linear 0 9)
      )
    ,
      ( 1
      , Gen.subtermM exp \e -> IR.objectProp e <$> genPropName
      )
    ,
      ( 2
      , IR.array <$> Gen.list (Range.linear 1 10) exp
      )
    ,
      ( 2
      , IR.object <$> Gen.list (Range.linear 1 10) ((,) <$> genPropName <*> exp)
      )
    ,
      ( 1
      , Gen.subtermM exp \e ->
          IR.wrapExpF . IR.ObjectUpdate e
            <$> Gen.nonEmpty
              (Range.linear 1 10)
              ((,) <$> genPropName <*> exp)
      )
    ,
      ( 5
      , Gen.subtermM exp \e -> (`IR.abstraction` e) <$> argument
      )
    ,
      ( 6
      , Gen.subtermM exp \e ->
          (`IR.lets` e) <$> Gen.nonEmpty (Range.linear 1 5) binding
      )
    ]

binding :: MonadGen m => m IR.Binding
binding = Gen.frequency [(8, standaloneBinding), (2, recursiveBinding)]

namedExp :: MonadGen m => m (IR.Name, IR.Exp)
namedExp = (,) <$> name <*> exp

recursiveBinding :: MonadGen m => m IR.Binding
recursiveBinding =
  IR.RecursiveGroup <$> Gen.nonEmpty (Range.linear 1 5) namedExp

standaloneBinding :: MonadGen m => m IR.Binding
standaloneBinding = IR.Standalone <$> namedExp

nonRecursiveExp :: MonadGen m => m IR.Exp
nonRecursiveExp =
  IR.wrapExpF
    <$> Gen.frequency
      [ (5, IR.Lit <$> literalNonRecursive)
      , (1, IR.Exception <$> Gen.text (Range.linear 0 10) Gen.unicode)
      ,
        ( 1
        , IR.Ctor
            <$> Gen.enumBounded
            <*> tyName
            <*> ctorName
            <*> Gen.list (Range.linear 0 10) fieldName
        )
      , (3, IR.RefFree <$> qualified name)
      ]

literalNonRecursiveExp :: MonadGen m => m IR.Exp
literalNonRecursiveExp = IR.wrapExpF . IR.Lit <$> literalNonRecursive

scalarExp :: MonadGen m => m IR.Exp
scalarExp = IR.wrapExpF . IR.Lit <$> scalarLiteral

literalNonRecursive :: MonadGen m => m (IR.Literal IR.Exp)
literalNonRecursive =
  Gen.frequency
    [ (5, scalarLiteral)
    , (1, pure $ IR.Array [])
    , (1, pure $ IR.Object [])
    ]

scalarLiteral :: MonadGen m => m (IR.Literal IR.Exp)
scalarLiteral =
  Gen.choice
    [ IR.Integer <$> Gen.integral (Range.exponential 0 1000)
    , IR.String <$> Gen.text (Range.linear 0 10) Gen.unicode
    , IR.Boolean <$> Gen.bool
    , IR.Char <$> Gen.unicode
    , IR.Floating <$> Gen.double (Range.exponentialFloat 0 1000000000000000000)
    ]

argument :: MonadGen m => m IR.Argument
argument =
  Gen.frequency
    [ (1, pure IR.ArgUnused)
    , (1, pure IR.ArgAnonymous)
    , (8, IR.ArgNamed <$> name)
    ]

qualified :: MonadGen m => m a -> m (IR.Qualified a)
qualified q =
  Gen.frequency
    [ (8, IR.Local <$> q)
    , (2, IR.Imported <$> moduleName <*> q)
    ]

refFreeLocal :: MonadGen m => m IR.Exp
refFreeLocal = IR.refFreeLocal <$> name

moduleName :: MonadGen m => m IR.ModuleName
moduleName = IR.ModuleName <$> Gen.element Corpus.colours

name :: MonadGen m => m IR.Name
name = IR.Name <$> Gen.element ["x", "y", "z", "i", "j", "k", "l"]

tyName :: MonadGen m => m IR.TyName
tyName = IR.TyName . Text.toTitle <$> Gen.element Corpus.waters

ctorName :: MonadGen m => m IR.CtorName
ctorName = IR.CtorName . Text.toTitle <$> Gen.element Corpus.colours

genPropName :: MonadGen m => m IR.PropName
genPropName = IR.PropName <$> Gen.element Corpus.metasyntactic

fieldName :: MonadGen m => m IR.FieldName
fieldName = IR.FieldName <$> Gen.element Corpus.metasyntactic
