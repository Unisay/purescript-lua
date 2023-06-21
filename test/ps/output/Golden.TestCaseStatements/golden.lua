local Golden_TestValues_I_f = function() return true end
local Golden_TestCaseStatements_I_J = function(value0)
  return { ["$ctor"] = "Golden_TestCaseStatements.J", value0 = value0 }
end
local Golden_TestCaseStatements_I_N = function()
  return { ["$ctor"] = "Golden_TestCaseStatements.N" }
end
local Golden_TestCaseStatements_I_multipleGuards = 1
local Golden_TestCaseStatements_I_d = function(m)
  return function(n)
    return function(x)
      return (function()
        local v = function()
          return (function() if "y" == x then return 0 else return 1 end end)()
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
end
local Golden_TestCaseStatements_I_b = "b"
local Golden_TestCaseStatements_I_a = 1
local Golden_TestCaseStatements_I_c = (function()
  local v = function()
    return (function()
      if 2 == Golden_TestCaseStatements_I_a then
        return (function()
          if Golden_TestValues_I_f(0) then
            return 10
          else
            return (function()
              if 3 == Golden_TestCaseStatements_I_a then
                return (function()
                  if Golden_TestValues_I_f(Golden_TestCaseStatements_I_a) then
                    return Golden_TestCaseStatements_I_a
                  else
                    return (function()
                      if 4 == Golden_TestCaseStatements_I_a then
                        return (function()
                          if Golden_TestValues_I_f(Golden_TestCaseStatements_I_a) then
                            return Golden_TestCaseStatements_I_a
                          else
                            return 0
                          end
                        end)()
                      else
                        return 0
                      end
                    end)()
                  end
                end)()
              else
                return (function()
                  if 4 == Golden_TestCaseStatements_I_a then
                    return (function()
                      if Golden_TestValues_I_f(Golden_TestCaseStatements_I_a) then
                        return Golden_TestCaseStatements_I_a
                      else
                        return 0
                      end
                    end)()
                  else
                    return 0
                  end
                end)()
              end
            end)()
          end
        end)()
      else
        return (function()
          if 3 == Golden_TestCaseStatements_I_a then
            return (function()
              if Golden_TestValues_I_f(Golden_TestCaseStatements_I_a) then
                return Golden_TestCaseStatements_I_a
              else
                return (function()
                  if 4 == Golden_TestCaseStatements_I_a then
                    return (function()
                      if Golden_TestValues_I_f(Golden_TestCaseStatements_I_a) then
                        return Golden_TestCaseStatements_I_a
                      else
                        return 0
                      end
                    end)()
                  else
                    return 0
                  end
                end)()
              end
            end)()
          else
            return (function()
              if 4 == Golden_TestCaseStatements_I_a then
                return (function()
                  if Golden_TestValues_I_f(Golden_TestCaseStatements_I_a) then
                    return Golden_TestCaseStatements_I_a
                  else
                    return 0
                  end
                end)()
              else
                return 0
              end
            end)()
          end
        end)()
      end
    end)()
  end
  return (function()
    if 1 == Golden_TestCaseStatements_I_a then
      return (function()
        if "b" == Golden_TestCaseStatements_I_b then
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
        else
          return v(true)
        end
      end)()
    else
      return v(true)
    end
  end)()
end)()
