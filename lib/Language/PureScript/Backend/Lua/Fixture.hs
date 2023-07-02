{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.Fixture where

import Language.PureScript.Backend.Lua.Name (Name, name)
import Language.PureScript.Backend.Lua.Name qualified as Name
import Language.PureScript.Backend.Lua.Types hiding (var)

--------------------------------------------------------------------------------
-- Hard-coded Lua pieces -------------------------------------------------------

prim :: Statement
prim = local1 (Name.join2 [name|Prim|] [name|undefined|]) Nil

runtimeLazy :: Statement
runtimeLazy = local1 [name|_S___runtime_lazy|] do
  let fun :: Name -> [Statement] -> Exp
      fun n = Function [((), ParamNamed n)] . fmap ann
      var :: Name -> Var
      var = VarName
      ret :: Exp -> Statement
      ret = Return . ann
  fun [name|name|] . pure . ret . fun [name|init|] $
    [ local1 [name|state|] (Integer 0)
    , local1 [name|val|] Nil
    , ret . functionDef [] $
        [ ifThenElse
            (varName [name|state|] `equalTo` Integer 2)
            [ret (varName [name|val|])]
            [ ifThenElse
                (varName [name|state|] `equalTo` Integer 1)
                ( pure . ret $
                    functionCall
                      (varName [name|error|])
                      [ binOp
                          Concat
                          (varName [name|name|])
                          ( String
                              " was needed before it finished initializing"
                          )
                      ]
                )
                [ var [name|state|] `assign` Integer 1
                , var [name|val|] `assign` functionCall (varName [name|init|]) []
                , var [name|state|] `assign` Integer 2
                , ret (varName [name|val|])
                ]
            ]
        ]
    ]
