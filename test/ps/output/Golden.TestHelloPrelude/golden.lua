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
  Apply0 = function(unused2) return Effect_I__S___lazy_applyEffect(0) end
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
  Apply0 = function(unused3) return Effect_I__S___lazy_applyEffect(0) end
}
Effect_I__S___lazy_functorEffect = (function(name)
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
end)("functorEffect")(function(unused4)
  return {
    map = (function(dictApplicative)
      return function(f)
        return function(a)
          return (function(dict)
            return dict.apply
          end)(dictApplicative.Apply0(nil))((function(dict)
            return dict.pure
          end)(dictApplicative)(f))(a)
        end
      end
    end)(Effect_I_applicativeEffect)
  }
end)
Effect_I__S___lazy_applyEffect = (function(name)
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
end)("applyEffect")(function(unused6)
  return {
    apply = (function(dictMonad)
      return function(f)
        local bind = (function(dict) return dict.bind end)(dictMonad.Bind1(nil))
        return function(a)
          return bind(f)(function(fPrime)
            return bind(a)(function(aPrime)
              return (function(dict)
                return dict.pure
              end)(dictMonad.Applicative0(nil))(fPrime(aPrime))
            end)
          end)
        end
      end
    end)(Effect_I_monadEffect),
    Functor0 = function(unused5) return Effect_I__S___lazy_functorEffect(0) end
  }
end)
return {
  main = (function(dictApplicative)
    return (function(dict) return dict.pure end)(dictApplicative)(((function()
      return {unit = nil}
    end)()).unit)
  end)(Effect_I_applicativeEffect)
}
