module Language.PureScript.Backend.AppOrModule where

import Language.PureScript.CoreFn qualified as Cfn

data AppOrModule
  = AsApplication Cfn.ModuleName Cfn.Ident
  | AsModule Cfn.ModuleName
  deriving stock (Show)

entryPointModule ∷ AppOrModule → Cfn.ModuleName
entryPointModule = \case
  AsApplication modname _ident → modname
  AsModule modname → modname
