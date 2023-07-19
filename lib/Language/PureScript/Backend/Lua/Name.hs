module Language.PureScript.Backend.Lua.Name
  ( module Reexport
  , specialNameType
  , specialNameCtor
  , join2
  ) where

import Language.Lua.Name as Reexport
import Language.Lua.Name qualified as Name

specialNameType ∷ Name
specialNameType = Name.unsafeFromText "$type"

specialNameCtor ∷ Name
specialNameCtor = Name.unsafeFromText "$ctor"

join2 ∷ Name → Name → Name
join2 a b = Name.unsafeFromText (Name.toText a <> "_I_" <> Name.toText b)
