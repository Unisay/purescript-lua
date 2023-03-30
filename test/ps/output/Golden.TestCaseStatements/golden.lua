local Golden_TestValues_I_f = function() return true end
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
      local v3 = function() if "y" == x2 then return 0 else return 1 end end
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
  local v5 = function()
    if 2 == Golden_TestCaseStatements_I_a then
      return (function()
        if Golden_TestValues_I_f(0) then
          return 10
        else
          return (function()
            if 3 == Golden_TestCaseStatements_I_a then
              return (function()
                local n18 = (function()
                  if 4 == Golden_TestCaseStatements_I_a then
                    return (function()
                      local n211 = 0
                      local y12 = Golden_TestCaseStatements_I_a
                      local z13 = Golden_TestCaseStatements_I_a
                      return (function()
                        if Golden_TestValues_I_f(y12) then
                          return z13
                        else
                          return n211
                        end
                      end)()
                    end)()
                  else
                    return 0
                  end
                end)()
                local y9 = Golden_TestCaseStatements_I_a
                local z10 = Golden_TestCaseStatements_I_a
                return (function()
                  if Golden_TestValues_I_f(z10) then
                    return y9
                  else
                    return n18
                  end
                end)()
              end)()
            else
              return (function()
                if 4 == Golden_TestCaseStatements_I_a then
                  return (function()
                    local n314 = 0
                    local y15 = Golden_TestCaseStatements_I_a
                    local z16 = Golden_TestCaseStatements_I_a
                    return (function()
                      if Golden_TestValues_I_f(y15) then
                        return z16
                      else
                        return n314
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
            local n417 = (function()
              if 4 == Golden_TestCaseStatements_I_a then
                return (function()
                  local n520 = 0
                  local y21 = Golden_TestCaseStatements_I_a
                  local z22 = Golden_TestCaseStatements_I_a
                  return (function()
                    if Golden_TestValues_I_f(y21) then
                      return z22
                    else
                      return n520
                    end
                  end)()
                end)()
              else
                return 0
              end
            end)()
            local y18 = Golden_TestCaseStatements_I_a
            local z19 = Golden_TestCaseStatements_I_a
            return (function()
              if Golden_TestValues_I_f(z19) then return y18 else return n417 end
            end)()
          end)()
        else
          return (function()
            if 4 == Golden_TestCaseStatements_I_a then
              return (function()
                local n623 = 0
                local y24 = Golden_TestCaseStatements_I_a
                local z25 = Golden_TestCaseStatements_I_a
                return (function()
                  if Golden_TestValues_I_f(y24) then
                    return z25
                  else
                    return n623
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
            local e76 = Golden_TestValues_I_f(2)
            return (function()
              if true == e76 then
                return (function()
                  local e87 = Golden_TestValues_I_f(1)
                  return (function()
                    if true == e87 then return 42 else return v5(true) end
                  end)()
                end)()
              else
                return v5(true)
              end
            end)()
          end)()
        else
          return v5(true)
        end
      end)()
    else
      return v5(true)
    end
  end)()
end)()
