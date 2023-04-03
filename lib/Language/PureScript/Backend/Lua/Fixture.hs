{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Language.PureScript.Backend.Lua.Fixture where

import Language.PureScript.Backend.Lua.Name (name)
import Language.PureScript.Backend.Lua.Types

--------------------------------------------------------------------------------
-- Hard-coded Lua pieces -------------------------------------------------------

primModule :: Module
primModule =
  Module
    { moduleChunk = [local1 [name|undefined|] Nil]
    , moduleName = ModuleName [name|Prim|]
    , moduleImports = []
    , moduleExports = [[name|undefined|]]
    , moduleForeigns = []
    , modulePath = "Prim"
    }

runtimeLazy :: Statement
runtimeLazy = local1 [name|_S___runtime_lazy|] do
  let var = Var . vname
      vname = VarName . LocalName
      fun n = Function [n]
  fun [name|name|] . pure . Return . fun [name|init|] $
    [ local1 [name|state|] (Integer 0)
    , local1 [name|val|] Nil
    , Return . fun [name|lineNumber|] $
        [ IfThenElse
            (var [name|state|] `equalTo` Integer 2)
            (Return (var [name|val|]) :| [])
            [
              ( var [name|state|] `equalTo` Integer 1
              , pure . Return $
                  FunctionCall
                    (var [name|error|])
                    [ BinOp
                        Concat
                        (var [name|name|])
                        (String " was needed before it finished initializing")
                    ]
              )
            ]
            ( Just $
                (vname [name|state|] `assign1` Integer 1)
                  :| [ vname [name|val|]
                        `assign1` FunctionCall (var [name|init|]) []
                     , vname [name|state|] `assign1` Integer 2
                     , Return (var [name|val|])
                     ]
            )
        ]
    ]
