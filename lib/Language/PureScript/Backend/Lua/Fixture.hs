{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.Fixture where

import Data.String.Interpolate (__i)
import Language.Lua.Types hiding (var)
import Language.PureScript.Backend.Lua.Name (Name, name)
import Language.PureScript.Backend.Lua.Name qualified as Name

--------------------------------------------------------------------------------
-- Hard-coded Lua pieces -------------------------------------------------------

prim ∷ Statement
prim = local1 (Name.join2 [name|Prim|] [name|undefined|]) Nil

runtimeLazyName ∷ Name
runtimeLazyName = [name|_S___runtime_lazy|]

runtimeLazy ∷ Statement
runtimeLazy =
  ForeignSourceCode
    [__i|
      local function #{Name.toText runtimeLazyName}(name)
        return function(init)
          return function()
            local state = 0
            local val = nil
            if state == 2 then
              return val
            else
              if state == 1 then
                return error(name .. " was needed before it finished initializing")
              else
                state = 1
                val = init()
                state = 2
                return val
              end
            end
          end
        end
      end
    |]

objectUpdateName ∷ Name
objectUpdateName = [name|_S___object_update|]

objectUpdate ∷ Statement
objectUpdate =
  ForeignSourceCode
    [__i|
      local function #{Name.toText objectUpdateName}(o, patches)
        local o_copy = {}
        for k, v in pairs(o) do
          local patch_v = patches
          if patch_v ~= nil then
            o_copy[k] = patch_v
          else
            o_copy[k] = v
          end
        end
        return o_copy
      end
    |]
