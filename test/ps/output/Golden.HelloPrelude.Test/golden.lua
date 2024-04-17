local Prim_I_undefined = nil
local function _S___runtime_lazy(name)
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
local Effect_I_foreign = {
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
    end,
  untilE = function(f)
      return function()
        while not f() do
        end
      end
    end,
  whileE = function(f)
      return function(a)
        return function()
          while f() do
            a()
          end
        end
      end
    end,
  forE = function(lo)
      return function(hi)
        return function(f)
          return function()
            for i = lo, hi do
              f(i)()
            end
          end
        end
      end
    end,
  foreachE = function(as)
      return function(f)
        return function()
          for i, v in ipairs(as) do
            f(v)()
          end
        end
      end
    end
}
local Control_Applicative_I_pure = function(dict) return dict.pure end
local Effect_I_monadEffect
local Effect_I_bindEffect
local Effect_I_applicativeEffect
local Effect_I__S___lazy_functorEffect
local Effect_I__S___lazy_applyEffect
Effect_I_monadEffect = {
  Applicative0 = function(unused0) return Effect_I_applicativeEffect end,
  Bind1 = function(unused1) return Effect_I_bindEffect end
}
Effect_I_bindEffect = {
  bind = Effect_I_foreign.bindE,
  Apply0 = function(unused2) return Effect_I__S___lazy_applyEffect(0) end
}
Effect_I_applicativeEffect = {
  pure = Effect_I_foreign.pureE,
  Apply0 = function(unused3) return Effect_I__S___lazy_applyEffect(0) end
}
Effect_I__S___lazy_functorEffect = _S___runtime_lazy("functorEffect")(function( unused4 )
  return {
    map = function(f)
      return (Effect_I_applicativeEffect.Apply0(Prim_I_undefined)).apply(Control_Applicative_I_pure(Effect_I_applicativeEffect)(f))
    end
  }
end)
Effect_I__S___lazy_applyEffect = _S___runtime_lazy("applyEffect")(function( unused6 )
  return {
    apply = (function()
      return function(f)
        local bind = (Effect_I_monadEffect.Bind1(Prim_I_undefined)).bind
        return function(a)
          return bind(f)(function(fPrime)
            return bind(a)(function(aPrime)
              return Control_Applicative_I_pure(Effect_I_monadEffect.Applicative0(Prim_I_undefined))(fPrime(aPrime))
            end)
          end)
        end
      end
    end)(),
    Functor0 = function(unused5) return Effect_I__S___lazy_functorEffect(0) end
  }
end)
return { main = Control_Applicative_I_pure(Effect_I_applicativeEffect)(nil) }
