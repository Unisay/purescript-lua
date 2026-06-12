local function PSLUA_runtime_lazy(name)
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
local M = {}
M.Data_Unit_foreign = { unit = {} }
M.Effect_foreign = {
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
M.Control_Applicative_pure = function(dict) return dict.pure end
M.Control_Bind_bind = function(dict) return dict.bind end
M.Effect_monadEffect = {
  Applicative0 = function() return M.Effect_applicativeEffect end,
  Bind1 = function() return M.Effect_bindEffect end
}
M.Effect_bindEffect = {
  bind = M.Effect_foreign.bindE,
  Apply0 = function() return M.Effect_Lazy_applyEffect(0) end
}
M.Effect_applicativeEffect = {
  pure = M.Effect_foreign.pureE,
  Apply0 = function() return M.Effect_Lazy_applyEffect(0) end
}
M.Effect_Lazy_functorEffect = PSLUA_runtime_lazy("functorEffect")(function()
  return {
    map = function(f)
      return function(a)
        return (M.Effect_applicativeEffect.Apply0()).apply(M.Control_Applicative_pure(M.Effect_applicativeEffect)(f))(a)
      end
    end
  }
end)
M.Effect_Lazy_applyEffect = PSLUA_runtime_lazy("applyEffect")(function()
  return {
    apply = (function()
      return function(f)
        local bind = M.Control_Bind_bind(M.Effect_monadEffect.Bind1())
        return function(a)
          return bind(f)(function(fPrime)
            return bind(a)(function(aPrime)
              return M.Control_Applicative_pure(M.Effect_monadEffect.Applicative0())(fPrime(aPrime))
            end)
          end)
        end
      end
    end)(),
    Functor0 = function() return M.Effect_Lazy_functorEffect(0) end
  }
end)
M.Golden_Issue37_Test_discard = function(dictBind)
  return M.Control_Bind_bind(dictBind)
end
return {
  baz = (function()
    return function(f)
      local Bind1 = M.Effect_monadEffect.Bind1()
      local pure = M.Control_Applicative_pure(M.Effect_monadEffect.Applicative0())
      return M.Golden_Issue37_Test_discard(Bind1)(f)(function()
        return M.Control_Bind_bind(Bind1)(pure({
          [1] = (function()
            return function(fn1)
              local Bind11 = M.Effect_monadEffect.Bind1()
              local discard1 = M.Golden_Issue37_Test_discard(Bind11)
              return M.Control_Bind_bind(M.Effect_monadEffect.Bind1())(fn1)(function(  )
                return discard1(fn1)(function()
                  return discard1(fn1)(function() return fn1 end)
                end)
              end)
            end
          end)()(f)
        }))(function() return pure(M.Data_Unit_foreign.unit) end)
      end)
    end
  end)()(M.Control_Applicative_pure(M.Effect_applicativeEffect)(M.Data_Unit_foreign.unit))
}
