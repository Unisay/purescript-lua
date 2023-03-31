local _S___runtime_lazy = function(name)
  return function(init)
    local state = 0
    local val = nil
    return function(lineNumber)
      if state == 2 then
        return val
      elseif state == 1 then
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
local Control_Apply_I_apply = function(dict0)
  local v1 = dict0
  return v1.apply
end
local Prim_I_undefined = nil
local Control_Applicative_I_pure = function(dict0)
  local v1 = dict0
  return v1.pure
end
local Control_Applicative_I_liftA1 = function(dictApplicative2)
  return function(f5)
    local apply3 = Control_Apply_I_apply(dictApplicative2.Apply0(Prim_I_undefined))
    local pure14 = Control_Applicative_I_pure(dictApplicative2)
    return function(a6) return apply3(pure14(f5))(a6) end
  end
end
local Control_Bind_I_bind = function(dict0) local v1 = dict0 return v1.bind end
local Control_Monad_I_ap = function(dictMonad0)
  return function(f3)
    local bind1 = Control_Bind_I_bind(dictMonad0.Bind1(Prim_I_undefined))
    local pure2 = Control_Applicative_I_pure(dictMonad0.Applicative0(Prim_I_undefined))
    return function(a4)
      return bind1(f3)(function(fPrime5)
        return bind1(a4)(function(aPrime6) return pure2(fPrime5(aPrime6)) end)
      end)
    end
  end
end
local Data_Unit_I_foreign = (function() return { unit = nil }  end)()
local Data_Unit_I_unit = Data_Unit_I_foreign.unit
local Effect_I_foreign = (function()
  return {

    pureE = function(a)
      return function()
        return a
      end
    end

    , bindE = function(a)
      return function(f)
        return function()
          return f(a())()
        end
      end
    end

    , untilE = function(f)
      return function()
        while not f() do end
      end
    end

    , whileE = function(f)
      return function(a)
        return function()
          while f() do
            a()
          end
        end
      end
    end

    , forE = function(lo)
      return function(hi)
        return function(f)
          return function()
            for i = lo, hi do
              f(i)()
            end
          end
        end
      end
    end

    , foreachE = function(as)
      return function(f)
        return function()
          for i, v in ipairs(as) do
            f(v)()
          end
        end
      end
    end

  }

end)()
local Effect_I_pureE = Effect_I_foreign.pureE
local Effect_I_bindE = Effect_I_foreign.bindE
local Effect_I_monadEffect, Effect_I_bindEffect, Effect_I_applicativeEffect, Effect_I__S___lazy_functorEffect, Effect_I__S___lazy_applyEffect, Effect_I_functorEffect, Effect_I_applyEffect
Effect_I_monadEffect = {
  Applicative0 = function() return Effect_I_applicativeEffect end,
  Bind1 = function() return Effect_I_bindEffect end
}
Effect_I_bindEffect = {
  bind = Effect_I_bindE,
  Apply0 = function() return Effect_I__S___lazy_applyEffect(0) end
}
Effect_I_applicativeEffect = {
  pure = Effect_I_pureE,
  Apply0 = function() return Effect_I__S___lazy_applyEffect(0) end
}
Effect_I__S___lazy_functorEffect = _S___runtime_lazy("functorEffect")(function()
  return { map = Control_Applicative_I_liftA1(Effect_I_applicativeEffect) }
end)
Effect_I__S___lazy_applyEffect = _S___runtime_lazy("applyEffect")(function()
  return {
    apply = Control_Monad_I_ap(Effect_I_monadEffect),
    Functor0 = function() return Effect_I__S___lazy_functorEffect(0) end
  }
end)
Effect_I_functorEffect = Effect_I__S___lazy_functorEffect(0)
Effect_I_applyEffect = Effect_I__S___lazy_applyEffect(0)
local Prelude_I_pass = function(dictApplicative0)
  return Control_Applicative_I_pure(dictApplicative0)(Data_Unit_I_unit)
end
local Golden_TestHelloPrelude_I_main = Prelude_I_pass(Effect_I_applicativeEffect)
