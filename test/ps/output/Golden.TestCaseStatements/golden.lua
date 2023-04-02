local Golden_TestValues_I_f = function(v0) return true end
local Golden_TestCaseStatements_I_J = function(value0)
  return { ["$ctor"] = "Golden_TestCaseStatements.J", value0 = value0 }
end
local Golden_TestCaseStatements_I_N = function()
  return { ["$ctor"] = "Golden_TestCaseStatements.N" }
end
local Golden_TestCaseStatements_I_multipleGuards = 1
local Golden_TestCaseStatements_I_d = function(m0)
  return function(n1)
    return function(x2)
      local v3 = function(v15) if "y" == x2 then return 0 else return 1 end end
      return (function()
        if "x" == x2 then
          return (function()
            if "Golden.TestCaseStatements.J" == m0["$ctor"] then
              return (function()
                local y4 = m0[0]
                return (function()
                  if "Golden.TestCaseStatements.N" == n1["$ctor"] then
                    return y4
                  else
                    return v3(true)
                  end
                end)()
              end)()
            else
              return v3(true)
            end
          end)()
        else
          return v3(true)
        end
      end)()
    end
  end
end
local Golden_TestCaseStatements_I_b = "b"
local Golden_TestCaseStatements_I_a = 1
local Golden_TestCaseStatements_I_c = (function()
  local v6 = function(v19)
    if 2 == Golden_TestCaseStatements_I_a then
      return (function()
        if Golden_TestValues_I_f(0) then
          return 10
        else
          return (function()
            if 3 == Golden_TestCaseStatements_I_a then
              return (function()
                local n110 = (function()
                  if 4 == Golden_TestCaseStatements_I_a then
                    return (function()
                      local n213 = 0
                      local y14 = Golden_TestCaseStatements_I_a
                      local z15 = Golden_TestCaseStatements_I_a
                      return (function()
                        if Golden_TestValues_I_f(y14) then
                          return z15
                        else
                          return n213
                        end
                      end)()
                    end)()
                  else
                    return 0
                  end
                end)()
                local y11 = Golden_TestCaseStatements_I_a
                local z12 = Golden_TestCaseStatements_I_a
                return (function()
                  if Golden_TestValues_I_f(z12) then
                    return y11
                  else
                    return n110
                  end
                end)()
              end)()
            else
              return (function()
                if 4 == Golden_TestCaseStatements_I_a then
                  return (function()
                    local n316 = 0
                    local y17 = Golden_TestCaseStatements_I_a
                    local z18 = Golden_TestCaseStatements_I_a
                    return (function()
                      if Golden_TestValues_I_f(y17) then
                        return z18
                      else
                        return n316
                      end
                    end)()
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
            local n419 = (function()
              if 4 == Golden_TestCaseStatements_I_a then
                return (function()
                  local n522 = 0
                  local y23 = Golden_TestCaseStatements_I_a
                  local z24 = Golden_TestCaseStatements_I_a
                  return (function()
                    if Golden_TestValues_I_f(y23) then
                      return z24
                    else
                      return n522
                    end
                  end)()
                end)()
              else
                return 0
              end
            end)()
            local y20 = Golden_TestCaseStatements_I_a
            local z21 = Golden_TestCaseStatements_I_a
            return (function()
              if Golden_TestValues_I_f(z21) then return y20 else return n419 end
            end)()
          end)()
        else
          return (function()
            if 4 == Golden_TestCaseStatements_I_a then
              return (function()
                local n625 = 0
                local y26 = Golden_TestCaseStatements_I_a
                local z27 = Golden_TestCaseStatements_I_a
                return (function()
                  if Golden_TestValues_I_f(y26) then
                    return z27
                  else
                    return n625
                  end
                end)()
              end)()
            else
              return 0
            end
          end)()
        end
      end)()
    end
  end
  return (function()
    if 1 == Golden_TestCaseStatements_I_a then
      return (function()
        if "b" == Golden_TestCaseStatements_I_b then
          return (function()
            local e77 = Golden_TestValues_I_f(2)
            return (function()
              if true == e77 then
                return (function()
                  local e88 = Golden_TestValues_I_f(1)
                  return (function()
                    if true == e88 then return 42 else return v6(true) end
                  end)()
                end)()
              else
                return v6(true)
              end
            end)()
          end)()
        else
          return v6(true)
        end
      end)()
    else
      return v6(true)
    end
  end)()
end)()
