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
M.Data_Semiring_foreign = {
  intAdd = function(x) return function(y) return x + y end end,
  intMul = function(x) return function(y) return x * y end end
}
M.Unsafe_Coerce_foreign = { unsafeCoerce = function(x) return x end }
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
M.Data_Semiring_semiringInt = {
  add = M.Data_Semiring_foreign.intAdd,
  zero = 0,
  mul = M.Data_Semiring_foreign.intMul,
  one = 1
}
M.Control_Applicative_pure = function(dict) return dict.pure end
M.Control_Bind_bind = function(dict) return dict.bind end
M.Data_Newtype_unwrap = function()
  return M.Unsafe_Coerce_foreign.unsafeCoerce
end
M.Data_Profunctor_composeFlipped = function(f)
  return function(g)
    return (function(f1)
      return function(g1) return function(x) return f1(g1(x)) end end
    end)(g)(f)
  end
end
M.Data_Profunctor_profunctorFn = {
  dimap = function(a2b)
    return function(c2d)
      return function(b2c)
        return M.Data_Profunctor_composeFlipped(a2b)(M.Data_Profunctor_composeFlipped(b2c)(c2d))
      end
    end
  end
}
M.Data_Profunctor_dimap = function(dict) return dict.dimap end
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
M.Golden_ProfunctorDictLens_Test_unwrap = M.Data_Newtype_unwrap()
M.Golden_ProfunctorDictLens_Test_discard = (function(dictBind)
  return M.Control_Bind_bind(dictBind)
end)(M.Effect_bindEffect)
M.Golden_ProfunctorDictLens_Test_logShow = function(a)
  return (function(s) return function() print(s) end end)((function(n) return tostring(n) end)(a))
end
M.Golden_ProfunctorDictLens_Test_Wrapped = function(x) return x end
M.Golden_ProfunctorDictLens_Test__Wrapped = function(dictProfunctor)
  return M.Data_Profunctor_dimap(dictProfunctor)(M.Golden_ProfunctorDictLens_Test_unwrap)(M.Golden_ProfunctorDictLens_Test_Wrapped)
end
M.Golden_ProfunctorDictLens_Test__Wrapped1 = M.Golden_ProfunctorDictLens_Test__Wrapped(M.Data_Profunctor_profunctorFn)
return M.Golden_ProfunctorDictLens_Test_discard(M.Golden_ProfunctorDictLens_Test_logShow(M.Golden_ProfunctorDictLens_Test_unwrap(M.Golden_ProfunctorDictLens_Test__Wrapped1(function( v )
  return M.Data_Semiring_semiringInt.add(v)(1)
end)(10))))(function()
  return M.Golden_ProfunctorDictLens_Test_discard(M.Golden_ProfunctorDictLens_Test_logShow(M.Golden_ProfunctorDictLens_Test_unwrap(M.Golden_ProfunctorDictLens_Test__Wrapped1(function( v )
    return M.Data_Semiring_semiringInt.mul(v)(2)
  end)(10))))(function()
    return M.Golden_ProfunctorDictLens_Test_logShow(M.Golden_ProfunctorDictLens_Test_unwrap(M.Data_Profunctor_dimap(M.Data_Profunctor_profunctorFn)(M.Data_Newtype_unwrap())(M.Unsafe_Coerce_foreign.unsafeCoerce)(function( v )
      return (function(x) return function(y) return x - y end end)(v)(5)
    end)(10)))
  end)
end)()
