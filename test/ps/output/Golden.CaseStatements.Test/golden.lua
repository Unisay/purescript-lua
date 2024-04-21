local M = {}
M.Golden_Values_Test_f = function() return true end
return {
  a = 1,
  b = "b",
  c = (function()
    local v = function() return 0 end
    if M.Golden_Values_Test_f(2) then
      if M.Golden_Values_Test_f(1) then return 42 else return v(true) end
    else
      return v(true)
    end
  end)(),
  J = function(value0)
    return { ["$ctor"] = "Golden.CaseStatements.Test∷M.J", value0 = value0 }
  end,
  N = { ["$ctor"] = "Golden.CaseStatements.Test∷M.N" },
  d = function(m)
    return function(n)
      return function(x)
        local v = function() if "y" == x then return 0 else return 1 end end
        if "x" == x then
          if "Golden.CaseStatements.Test∷M.J" == m["$ctor"] then
            if "Golden.CaseStatements.Test∷M.N" == n["$ctor"] then
              return m.value0
            else
              return v(true)
            end
          else
            return v(true)
          end
        else
          return v(true)
        end
      end
    end
  end,
  multipleGuards = 1
}
