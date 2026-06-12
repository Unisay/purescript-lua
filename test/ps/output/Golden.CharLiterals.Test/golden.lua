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
M.Effect_Console_foreign = {
  log = function(s) return function() print(s) end end
}
M.Data_Eq_eqChar = {
  eq = ((function()
    local refEq = function(r1) return function(r2) return r1 == r2 end end
    return { eqCharImpl = refEq }
  end)()).eqCharImpl
}
M.Data_Show_show = function(dict) return dict.show end
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
M.Golden_CharLiterals_Test_discard = (function(dictBind)
  return M.Control_Bind_bind(dictBind)
end)(M.Effect_bindEffect)
M.Golden_CharLiterals_Test_show = M.Data_Show_show({
  show = function(n)
      local code = n:byte()
      if code < 0x20 or code == 0x7F then
        if n == "\x07" then return "'\\a'" end
        if n == "\b" then return "'\\b'" end
        if n == "\f" then return "'\\f'" end
        if n == "\n" then return "'\\n'" end
        if n == "\r" then return "'\\r'" end
        if n == "\t" then return "'\\t'" end
        if n == "\v" then return "'\\v'" end
        return "'\\" .. code:toString(10) .. "'"
      end
      if n == "'" or n == "\\" then return "'\\" .. n .. "'" end
      return "'" .. n .. "'"
    end
})
M.Golden_CharLiterals_Test_show1 = M.Data_Show_show({
  show = function(v)
    if v then
      return "true"
    else
      if false == v then
        return "false"
      else
        return error("No patterns matched")
      end
    end
  end
})
return M.Golden_CharLiterals_Test_discard(M.Effect_Console_foreign.log(M.Golden_CharLiterals_Test_show("\n")))(function(  )
  return M.Golden_CharLiterals_Test_discard(M.Effect_Console_foreign.log(M.Golden_CharLiterals_Test_show("\t")))(function(  )
    return M.Golden_CharLiterals_Test_discard(M.Effect_Console_foreign.log(M.Golden_CharLiterals_Test_show("\r")))(function(  )
      return M.Golden_CharLiterals_Test_discard(M.Effect_Console_foreign.log(M.Golden_CharLiterals_Test_show("\'")))(function(  )
        return M.Golden_CharLiterals_Test_discard(M.Effect_Console_foreign.log(M.Golden_CharLiterals_Test_show("\\")))(function(  )
          return M.Golden_CharLiterals_Test_discard(M.Effect_Console_foreign.log(M.Golden_CharLiterals_Test_show("a")))(function(  )
            return M.Golden_CharLiterals_Test_discard(M.Effect_Console_foreign.log(M.Golden_CharLiterals_Test_show1(M.Data_Eq_eqChar.eq("\n")("\n"))))(function(  )
              return M.Effect_Console_foreign.log(M.Golden_CharLiterals_Test_show1((function(  )
                if "Data.Ordering∷Ordering.LT" == ((function()
                  local unsafeCoerceImpl = function(lt)
                    return function(eq)
                      return function(gt)
                        return function(x)
                          return function(y)
                            if x < y then
                              return lt
                            elseif x == y then
                              return eq
                            else
                              return gt
                            end
                          end
                        end
                      end
                    end
                  end
                  return { ordCharImpl = unsafeCoerceImpl }
                end)()).ordCharImpl({
                  ["$ctor"] = "Data.Ordering∷Ordering.LT"
                })({ ["$ctor"] = "Data.Ordering∷Ordering.EQ" })({
                  ["$ctor"] = "Data.Ordering∷Ordering.GT"
                })("\t")("\n")["$ctor"] then
                  return true
                else
                  return false
                end
              end)()))
            end)
          end)
        end)
      end)
    end)
  end)
end)()
