module Language.PureScript.Backend.IR.Gen where

import Data.Text qualified as Text
import Hedgehog (MonadGen)
import Hedgehog.Corpus qualified as Corpus
import Hedgehog.Gen.Extended qualified as Gen
import Hedgehog.Range qualified as Range
import Language.PureScript.Backend.IR.Names qualified as IR
import Language.PureScript.Backend.IR.Types (noAnn)
import Language.PureScript.Backend.IR.Types qualified as IR
import Language.PureScript.Names (ModuleName, moduleNameFromString)
import Prelude hiding (exp)

exp ∷ ∀ m. MonadGen m ⇒ m IR.Exp
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
      , Gen.subtermM exp \e →
          IR.arrayIndex e <$> Gen.integral (Range.linear 0 9)
      )
    ,
      ( 1
      , Gen.subtermM exp \e → IR.objectProp e <$> genPropName
      )
    ,
      ( 2
      , IR.literalArray <$> Gen.list (Range.linear 1 10) exp
      )
    ,
      ( 2
      , IR.literalObject <$> Gen.list (Range.linear 1 10) ((,) <$> genPropName <*> exp)
      )
    ,
      ( 1
      , Gen.subtermM exp \e →
          IR.objectUpdate e
            <$> Gen.nonEmpty (Range.linear 1 10) ((,) <$> genPropName <*> exp)
      )
    ,
      ( 5
      , Gen.subtermM exp \e → (`IR.abstraction` e) <$> parameter
      )
    ,
      ( 6
      , Gen.subtermM exp \e →
          (`IR.lets` e) <$> Gen.nonEmpty (Range.linear 1 5) binding
      )
    ]

binding ∷ MonadGen m ⇒ m IR.Binding
binding = Gen.frequency [(8, standaloneBinding), (2, recursiveBinding)]

namedExp ∷ MonadGen m ⇒ m (IR.Ann, IR.Name, IR.Exp)
namedExp = (noAnn,,) <$> name <*> exp

recursiveBinding ∷ MonadGen m ⇒ m IR.Binding
recursiveBinding =
  IR.RecursiveGroup <$> Gen.nonEmpty (Range.linear 1 5) namedExp

standaloneBinding ∷ MonadGen m ⇒ m IR.Binding
standaloneBinding = IR.Standalone <$> namedExp

nonRecursiveExp ∷ MonadGen m ⇒ m IR.Exp
nonRecursiveExp =
  Gen.frequency
    [ (5, literalNonRecursiveExp)
    , (1, exception)
    , (1, ctor)
    , (3, IR.ref <$> qualified name <*> pure 0)
    ]

exception ∷ MonadGen m ⇒ m IR.Exp
exception = IR.exception <$> Gen.text (Range.linear 0 10) Gen.unicode

ctor ∷ MonadGen m ⇒ m IR.Exp
ctor =
  IR.ctor
    <$> Gen.enumBounded
    <*> moduleName
    <*> tyName
    <*> ctorName
    <*> Gen.list (Range.linear 0 10) fieldName

literalNonRecursiveExp ∷ MonadGen m ⇒ m IR.Exp
literalNonRecursiveExp =
  Gen.frequency
    [ (5, scalarExp)
    , (1, pure $ IR.literalArray [])
    , (1, pure $ IR.literalObject [])
    ]

scalarExp ∷ MonadGen m ⇒ m IR.Exp
scalarExp =
  Gen.choice
    [ IR.literalInt <$> Gen.integral (Range.exponential 0 1000)
    , IR.literalString <$> Gen.text (Range.linear 0 10) Gen.unicode
    , IR.literalBool <$> Gen.bool
    , IR.literalChar <$> Gen.unicode
    , IR.literalFloat
        <$> Gen.double
          (Range.exponentialFloat 0 1000000000000000000)
    ]

parameter ∷ MonadGen m ⇒ m (IR.Parameter IR.Ann)
parameter =
  Gen.frequency
    [ (1, pure (IR.ParamUnused noAnn))
    , (9, IR.ParamNamed noAnn <$> name)
    ]

qualified ∷ MonadGen m ⇒ m a → m (IR.Qualified a)
qualified q =
  Gen.frequency
    [ (8, IR.Local <$> q)
    , (2, IR.Imported <$> moduleName <*> q)
    ]

refLocal ∷ MonadGen m ⇒ m IR.Exp
refLocal = flip IR.refLocal 0 <$> name

moduleName ∷ MonadGen m ⇒ m ModuleName
moduleName = moduleNameFromString <$> Gen.element Corpus.colours

name ∷ MonadGen m ⇒ m IR.Name
name = IR.Name <$> Gen.element ["x", "y", "z", "i", "j", "k", "l"]

tyName ∷ MonadGen m ⇒ m IR.TyName
tyName = IR.TyName . Text.toTitle <$> Gen.element Corpus.waters

ctorName ∷ MonadGen m ⇒ m IR.CtorName
ctorName = IR.CtorName . Text.toTitle <$> Gen.element Corpus.colours

genPropName ∷ MonadGen m ⇒ m IR.PropName
genPropName = IR.PropName <$> Gen.element Corpus.metasyntactic

fieldName ∷ MonadGen m ⇒ m IR.FieldName
fieldName = IR.FieldName <$> Gen.element Corpus.metasyntactic
