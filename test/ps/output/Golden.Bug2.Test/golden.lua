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
M.Effect_Exception_foreign = {
  error = function(msg) return msg end,
  throwException = function(err) return function() error(err) end end
}
M.Control_Applicative_pure = function(dict) return dict.pure end
M.Data_Maybe_Nothing = { ["$ctor"] = "Data.Maybe∷Maybe.Nothing" }
M.Data_Maybe_Just = function(value0)
  return { ["$ctor"] = "Data.Maybe∷Maybe.Just", value0 = value0 }
end
M.Data_Maybe_maybe = function(v, v1)
  return function(v2)
    if "Data.Maybe∷Maybe.Nothing" == v2["$ctor"] then
      return v
    else
      if "Data.Maybe∷Maybe.Just" == v2["$ctor"] then
        return v1(v2.value0)
      else
        return error("No patterns matched")
      end
    end
  end
end
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
      return (M.Effect_applicativeEffect.Apply0()).apply(M.Control_Applicative_pure(M.Effect_applicativeEffect)(f))
    end
  }
end)
M.Effect_Lazy_applyEffect = PSLUA_runtime_lazy("applyEffect")(function()
  return {
    apply = (function()
      return function(f)
        return function(a)
          local bind = (M.Effect_monadEffect.Bind1()).bind
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
return {
  main = M.Data_Maybe_maybe((function(f)
    return function(g) return function(x) return f(g(x)) end end
  end)(M.Effect_Exception_foreign.throwException)(M.Effect_Exception_foreign.error)("Some error"), M.Control_Applicative_pure(M.Effect_applicativeEffect), M.Data_Maybe_maybe(M.Data_Maybe_Nothing, M.Data_Maybe_Just, (function( v )
    return function(v1)
      if "Data.Maybe∷Maybe.Just" == v1["$ctor"] then
        return M.Data_Maybe_Just(v(v1.value0))
      else
        return M.Data_Maybe_Nothing
      end
    end
  end)(function(x) return x end)(M.Data_Maybe_Nothing)))
}
