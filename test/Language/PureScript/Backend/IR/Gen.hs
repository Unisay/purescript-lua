module Language.PureScript.Backend.IR.Gen where

import Data.Text qualified as Text
import Hedgehog (MonadGen)
import Hedgehog.Corpus qualified as Corpus
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.PureScript.Backend.IR.Types hiding (moduleName, qualified)
import Prelude hiding (exp)

exp :: forall m. MonadGen m => m Exp
exp =
  recursiveFrequency
    [(1, nonRecursiveExp)]
    [
      ( 7
      , Gen.subterm2 exp exp application
      )
    ,
      ( 3
      , Gen.subterm3 exp exp exp ifThenElse
      )
    ,
      ( 1
      , Gen.subtermM exp \e ->
          arrayIndex e <$> Gen.integral (Range.linear 0 9)
      )
    ,
      ( 1
      , Gen.subtermM exp \e -> objectProp e <$> genPropName
      )
    ,
      ( 2
      , array <$> Gen.list (Range.linear 1 10) exp
      )
    ,
      ( 2
      , object <$> Gen.list (Range.linear 1 10) ((,) <$> genPropName <*> exp)
      )
    ,
      ( 1
      , Gen.subtermM exp \e ->
          wrapExpF . ObjectUpdate e
            <$> Gen.nonEmpty
              (Range.linear 1 10)
              ((,) <$> genPropName <*> exp)
      )
    ,
      ( 5
      , Gen.subtermM exp \e -> (`abstraction` e) <$> argument
      )
    ,
      ( 6
      , Gen.subtermM exp \e ->
          (`lets` e) <$> Gen.nonEmpty (Range.linear 1 5) binding
      )
    ]

binding :: MonadGen m => m Binding
binding = Gen.frequency [(8, standaloneBinding), (2, recursiveBinding)]

namedExp :: MonadGen m => m (Name, Exp)
namedExp = (,) <$> name <*> exp

recursiveBinding :: MonadGen m => m Binding
recursiveBinding = RecursiveGroup <$> Gen.nonEmpty (Range.linear 1 5) namedExp

standaloneBinding :: MonadGen m => m Binding
standaloneBinding = Standalone <$> namedExp

recursiveFrequency :: MonadGen m => [(Int, m a)] -> [(Int, m a)] -> m a
recursiveFrequency nonrecur recur =
  Gen.sized $ \n ->
    if n <= 1
      then Gen.frequency nonrecur
      else Gen.frequency $ nonrecur <> fmap (fmap Gen.small) recur

nonRecursiveExp :: MonadGen m => m Exp
nonRecursiveExp =
  wrapExpF
    <$> Gen.frequency
      [ (5, Lit <$> literalNonRecursive)
      , (1, Exception <$> Gen.text (Range.linear 0 10) Gen.unicode)
      ,
        ( 1
        , Ctor
            <$> Gen.enumBounded
            <*> tyName
            <*> ctorName
            <*> Gen.list (Range.linear 0 10) fieldName
        )
      , (3, RefFree <$> qualified name)
      ]

literalNonRecursiveExp :: MonadGen m => m Exp
literalNonRecursiveExp = wrapExpF . Lit <$> literalNonRecursive

literalNonRecursive :: MonadGen m => m (Literal Exp)
literalNonRecursive =
  Gen.choice
    [ Integer <$> Gen.integral (Range.exponential 0 1000)
    , String <$> Gen.text (Range.linear 0 10) Gen.unicode
    , Boolean <$> Gen.bool
    , Char <$> Gen.unicode
    , Floating <$> Gen.double (Range.exponentialFloat 0 1000000000000000000)
    , pure $ Array []
    , pure $ Object []
    ]

argument :: MonadGen m => m Argument
argument =
  Gen.frequency
    [ (1, pure ArgUnused)
    , (1, pure ArgAnonymous)
    , (8, ArgNamed <$> name)
    ]

qualified :: MonadGen m => m a -> m (Qualified a)
qualified q =
  Gen.frequency
    [ (8, Local <$> q)
    , (2, Imported <$> moduleName <*> q)
    ]

moduleName :: MonadGen m => m ModuleName
moduleName = ModuleName <$> Gen.element Corpus.colours

name :: MonadGen m => m Name
name = Name <$> Gen.element ["x", "y", "z", "i", "j", "k", "l"]

tyName :: MonadGen m => m TyName
tyName = TyName . Text.toTitle <$> Gen.element Corpus.waters

ctorName :: MonadGen m => m CtorName
ctorName = CtorName . Text.toTitle <$> Gen.element Corpus.colours

genPropName :: MonadGen m => m PropName
genPropName = PropName <$> Gen.element Corpus.metasyntactic

fieldName :: MonadGen m => m FieldName
fieldName = FieldName <$> Gen.element Corpus.metasyntactic
