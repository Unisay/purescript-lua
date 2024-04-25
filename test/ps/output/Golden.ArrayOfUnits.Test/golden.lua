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
M.Data_Semiring_foreign = {
  intAdd = function(x) return function(y) return x + y end end,
  intMul = function(x) return function(y) return x * y end end
}
M.Data_Foldable_foreign = {
  foldrArray = function(f)
      return function(init)
        return function(xs)
          local acc = init
          local len = #xs
          for i = len, 1, -1 do acc = f(xs[i])(acc) end
          return acc
        end
      end
    end,
  foldlArray = function(f)
      return function(init)
        return function(xs)
          local acc = init
          local len = #xs
          for i = 1, len do acc = f(acc)(xs[i]) end
          return acc
        end
      end
    end
}
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
M.Control_Semigroupoid_semigroupoidFn = {
  compose = function(f)
    return function(g) return function(x) return f(g(x)) end end
  end
}
M.Data_Semiring_semiringInt = {
  add = M.Data_Semiring_foreign.intAdd,
  zero = 0,
  mul = M.Data_Semiring_foreign.intMul,
  one = 1
}
M.Control_Apply_apply = function(dict) return dict.apply end
M.Control_Applicative_pure = function(dict) return dict.pure end
M.Control_Bind_bind = function(dict) return dict.bind end
M.Data_Foldable_foldr = function(dict) return dict.foldr end
M.Data_Foldable_foldableArray = {
  foldr = M.Data_Foldable_foreign.foldrArray,
  foldl = M.Data_Foldable_foreign.foldlArray,
  foldMap = function(dictMonoid)
    return function(f)
      return M.Data_Foldable_foldr(M.Data_Foldable_foldableArray)(function(x)
        return (dictMonoid.Semigroup0()).append(f(x))
      end)(dictMonoid.mempty)
    end
  end
}
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
      return M.Control_Apply_apply(M.Effect_applicativeEffect.Apply0())(M.Control_Applicative_pure(M.Effect_applicativeEffect)(f))
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
M.Effect_Console_logShow = function(dictShow)
  return function(a)
    return (function(s) return function() print(s) end end)(dictShow.show(a))
  end
end
return (function()
  local arr = {
    [1] = M.Data_Unit_foreign.unit,
    [2] = M.Data_Unit_foreign.unit,
    [3] = M.Data_Unit_foreign.unit
  }
  return M.Control_Bind_bind(M.Effect_bindEffect)(M.Data_Foldable_foldr(M.Data_Foldable_foldableArray)(M.Control_Semigroupoid_semigroupoidFn.compose(function( a )
    return M.Control_Apply_apply(M.Effect_applicativeEffect.Apply0())(((M.Effect_applicativeEffect.Apply0()).Functor0()).map(function(  )
      return function(x) return x end
    end)(a))
  end)(M.Effect_Console_logShow({
    show = function() return "unit" end
  })))(M.Control_Applicative_pure(M.Effect_applicativeEffect)(M.Data_Unit_foreign.unit))(arr))(function(  )
    return M.Effect_Console_logShow({
      show = function(n) return tostring(n) end
    })(M.Data_Foldable_foldableArray.foldl(function(c)
      return function()
        return M.Data_Semiring_semiringInt.add(M.Data_Semiring_semiringInt.one)(c)
      end
    end)(M.Data_Semiring_semiringInt.zero)(arr))
  end)
end)()()
