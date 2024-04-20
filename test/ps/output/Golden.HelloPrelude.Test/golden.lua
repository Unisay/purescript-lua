PSLUA_Prim_undefined = nil
function PSLUA_runtime_lazy(name)
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
PSLUA_Effect_foreign = {
  pureE = function(a)
      return function()
        return a
      end
    end,
  bindE = function(a)
      return function(f)
        return function()
          return f(a())()
        end
      end
    end
}
PSLUA_Control_Applicative_pure = function(dict) return dict.pure end
local PSLUA_Effect_monadEffect
local PSLUA_Effect_bindEffect
local PSLUA_Effect_applicativeEffect
local PSLUA_Effect_PSLUA_lazy_functorEffect
local PSLUA_Effect_PSLUA_lazy_applyEffect
PSLUA_Effect_monadEffect = {
  Applicative0 = function(unused0) return PSLUA_Effect_applicativeEffect end,
  Bind1 = function(unused1) return PSLUA_Effect_bindEffect end
}
PSLUA_Effect_bindEffect = {
  bind = PSLUA_Effect_foreign.bindE,
  Apply0 = function(unused2) return PSLUA_Effect_PSLUA_lazy_applyEffect(0) end
}
PSLUA_Effect_applicativeEffect = {
  pure = PSLUA_Effect_foreign.pureE,
  Apply0 = function(unused3) return PSLUA_Effect_PSLUA_lazy_applyEffect(0) end
}
PSLUA_Effect_PSLUA_lazy_functorEffect = PSLUA_runtime_lazy("functorEffect")(function( unused4 )
  return {
    map = function(f)
      return (PSLUA_Effect_applicativeEffect.Apply0(PSLUA_Prim_undefined)).apply(PSLUA_Control_Applicative_pure(PSLUA_Effect_applicativeEffect)(f))
    end
  }
end)
PSLUA_Effect_PSLUA_lazy_applyEffect = PSLUA_runtime_lazy("applyEffect")(function( unused6 )
  return {
    apply = (function()
      return function(f)
        local bind = (PSLUA_Effect_monadEffect.Bind1(PSLUA_Prim_undefined)).bind
        return function(a)
          return bind(f)(function(fPrime)
            return bind(a)(function(aPrime)
              return PSLUA_Control_Applicative_pure(PSLUA_Effect_monadEffect.Applicative0(PSLUA_Prim_undefined))(fPrime(aPrime))
            end)
          end)
        end
      end
    end)(),
    Functor0 = function(unused5)
      return PSLUA_Effect_PSLUA_lazy_functorEffect(0)
    end
  }
end)
return {
  main = PSLUA_Control_Applicative_pure(PSLUA_Effect_applicativeEffect)(nil)
}
