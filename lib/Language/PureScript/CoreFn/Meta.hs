-- | Metadata annotations for core functional representation
module Language.PureScript.CoreFn.Meta where

import Language.PureScript.Names (Ident)

-- | Metadata annotations
data Meta
  = -- | The contained value is a data constructor
    IsConstructor ConstructorType [Ident]
  | -- | The contained value is a newtype
    IsNewtype
  | -- | The contained value is a typeclass dictionary constructor
    IsTypeClassConstructor
  | -- | The contained reference is for a foreign member
    IsForeign
  | -- | The contained value is a where clause
    IsWhere
  | -- | The contained function application was synthesized by the compiler
    IsSyntheticApp
  deriving stock (Show, Eq, Ord)

-- | Data constructor metadata
data ConstructorType = ProductType | SumType
  deriving stock (Show, Eq, Ord)
