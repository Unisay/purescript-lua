{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.Fixture where

import Data.String.Interpolate (__i)
import Language.PureScript.Backend.Lua.Name (Name, name, unsafeName)
import Language.PureScript.Backend.Lua.Name qualified as Name
import Language.PureScript.Backend.Lua.Types hiding (var)
import Language.PureScript.Backend.IR.Names (ModuleName(..))

--------------------------------------------------------------------------------
-- Hard-coded Lua pieces -------------------------------------------------------

prim ∷ Name
prim = [name|Prim|]

primModule :: ModuleName
primModule = ModuleName "Prim"

undefined ∷ Name
undefined = primName [name|undefined|]

primName ∷ Name → Name
primName = psluaName . Name.join2 prim

uniqueName ∷ MonadState Natural m ⇒ Text → m Name
uniqueName prefix = do
  index ← get
  modify' (+ 1)
  pure $ unsafeName (prefix <> show index)

psluaName ∷ Name → Name
psluaName = Name.join2 [name|PSLUA|]

runtimeLazyName ∷ Name
runtimeLazyName = psluaName [name|runtime_lazy|]

runtimeLazy ∷ Statement
runtimeLazy =
  ForeignSourceStat
    [__i| function #{Name.toText runtimeLazyName}(name)
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
objectUpdateName = psluaName [name|object_update|]

objectUpdate ∷ Statement
objectUpdate =
  ForeignSourceStat
    [__i|
      function #{Name.toText objectUpdateName}(o, patches)
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
