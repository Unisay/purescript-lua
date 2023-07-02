local Golden_TestValues_I_f = function(unused0) return true end
return {
  a = 1,
  b = "b",
  c = (function()
    local v = function(unused0) return 0 end
    return (function()
      if true == Golden_TestValues_I_f(2) then
        return (function()
          if true == Golden_TestValues_I_f(1) then
            return 42
          else
            return v(true)
          end
        end)()
      else
        return v(true)
      end
    end)()
  end)(),
  J = function(value0)
    return { ["$ctor"] = "Golden_TestCaseStatements.J", value0 = value0 }
  end,
  N = function() return { ["$ctor"] = "Golden_TestCaseStatements.N" } end,
  d = function(m)
    return function(n)
      return function(x)
        return (function()
          local v = function(unused0)
            return (function()
              if "y" == x then return 0 else return 1 end
            end)()
          end
          return (function()
            if "x" == x then
              return (function()
                if "Golden.TestCaseStatements.J" == m["$ctor"] then
                  return (function()
                    if "Golden.TestCaseStatements.N" == n["$ctor"] then
                      return m[0]
                    else
                      return v(true)
                    end
                  end)()
                else
                  return v(true)
                end
              end)()
            else
              return v(true)
            end
          end)()
        end)()
      end
    end
  end,
  multipleGuards = 1
}
