module Language.PureScript.Backend.IR.Names
  ( module Reexport
  , Name (..)
  , nameParser
  , QName (..)
  , printQName
  , TyName (..)
  , CtorName (..)
  , FieldName (..)
  , PropName (..)
  , Qualified (..)
  , qualifiedQName
  ) where

import Data.Char (isAlphaNum)
import Language.PureScript.CoreFn qualified as Cfn
import Language.PureScript.CoreFn.ModuleName as Reexport
  ( ModuleName
  , isBuiltinModuleName
  , moduleNameFromText
  , moduleNameToText
  , unsafeModuleNameFromText
  )
import Quiet (Quiet (..))
import Text.Megaparsec qualified as Megaparsec
import Prelude hiding (show)

newtype Name = Name {nameToText ∷ Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet Name)

nameParser ∷ Megaparsec.Parsec Void Text Name
nameParser = Name <$> Megaparsec.takeWhile1P (Just "name char") isAlphaNum

data QName = QName {qnameModuleName ∷ ModuleName, qnameName ∷ Name}
  deriving stock (Eq, Ord, Show)

printQName ∷ QName → Text
printQName QName {..} =
  Cfn.moduleNameToText qnameModuleName <> "∷" <> nameToText qnameName

newtype TyName = TyName {renderTyName ∷ Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet TyName)

newtype CtorName = CtorName {renderCtorName ∷ Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet CtorName)

-- TODO: is it used at all?
newtype FieldName = FieldName {renderFieldName ∷ Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet FieldName)

newtype PropName = PropName {renderPropName ∷ Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet PropName)

data Qualified a = Local a | Imported ModuleName a
  deriving stock (Show, Eq, Ord, Functor)

qualifiedQName ∷ QName → Qualified Name
qualifiedQName QName {qnameModuleName, qnameName} =
  Imported qnameModuleName qnameName
