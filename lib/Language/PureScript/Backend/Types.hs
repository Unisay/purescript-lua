module Language.PureScript.Backend.Types where

import Language.PureScript.Names qualified as PS

data AppOrModule
  = AsApplication PS.ModuleName PS.Ident
  | AsModule PS.ModuleName
  deriving stock (Show)

entryPointModule ∷ AppOrModule → PS.ModuleName
entryPointModule = \case
  AsApplication modname _ident → modname
  AsModule modname → modname
