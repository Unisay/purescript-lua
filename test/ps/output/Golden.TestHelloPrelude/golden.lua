local Effect_I_monadEffect
local Effect_I_bindEffect
local Effect_I_applicativeEffect
local Effect_I__S___lazy_functorEffect
local Effect_I__S___lazy_applyEffect
local Effect_I_functorEffect
local Effect_I_applyEffect
Effect_I_monadEffect = {
  Applicative0 = function() return Effect_I_applicativeEffect end,
  Bind1 = function() return Effect_I_bindEffect end
}
Effect_I_bindEffect = {
  bind = ((function()
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
  end)()).bindE,
  Apply0 = function() return Effect_I__S___lazy_applyEffect(0) end
}
Effect_I_applicativeEffect = {
  pure = ((function()
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
  end)()).pureE,
  Apply0 = function() return Effect_I__S___lazy_applyEffect(0) end
}
Effect_I__S___lazy_functorEffect = (function(name)
  return function(init)
    return function(lineNumber)
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
end)("functorEffect")(function()
  return {
    map = (function(dictApplicative2)
      return function(f5)
        return function(a6)
          return (function(dict0)
            return dict0.apply
          end)(dictApplicative2.Apply0(nil))((function(dict0)
            return dict0.pure
          end)(dictApplicative2)(f5))(a6)
        end
      end
    end)(Effect_I_applicativeEffect)
  }
end)
Effect_I__S___lazy_applyEffect = (function(name)
  return function(init)
    return function(lineNumber)
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
end)("applyEffect")(function()
  return {
    apply = (function(dictMonad0)
      return function(f3)
        return function(a4)
          local bind1 = (function(dict0)
            return dict0.bind
          end)(dictMonad0.Bind1(nil))
          return bind1(f3)(function(fPrime5)
            return bind1(a4)(function(aPrime6)
              return (function(dict0)
                return dict0.pure
              end)(dictMonad0.Applicative0(nil))(fPrime5(aPrime6))
            end)
          end)
        end
      end
    end)(Effect_I_monadEffect),
    Functor0 = function() return Effect_I__S___lazy_functorEffect(0) end
  }
end)
Effect_I_functorEffect = Effect_I__S___lazy_functorEffect(0)
Effect_I_applyEffect = Effect_I__S___lazy_applyEffect(0)
local Golden_TestHelloPrelude_I_main = (function(dictApplicative0)
  return (function(dict0) return dict0.pure end)(dictApplicative0)(((function()
    return { unit = nil }
  end)()).unit)
end)(Effect_I_applicativeEffect)
