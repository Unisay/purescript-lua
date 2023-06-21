local Golden_TestValues_I_f = function(v) return true end
local Golden_TestCaseStatements_I_J = function(value0)
  return { ["$ctor"] = "Golden_TestCaseStatements.J", value0 = value0 }
end
local Golden_TestCaseStatements_I_N = function()
  return { ["$ctor"] = "Golden_TestCaseStatements.N" }
end
local Golden_TestCaseStatements_I_multipleGuards = (function()
  if false then
    return 0
  else
    return (function()
      if true then return 1 else return error("No patterns matched") end
    end)()
  end
end)()
local Golden_TestCaseStatements_I_d = function(m)
  return function(n)
    return function(x)
      return (function()
        local v = function(v1)
          return (function() if "y" == x then return 0 else return 1 end end)()
        end
        return (function()
          if "x" == x then
            return (function()
              if "Golden.TestCaseStatements.J" == m["$ctor"] then
                return (function()
                  local y = m[0]
                  return (function()
                    if "Golden.TestCaseStatements.N" == n["$ctor"] then
                      return y
                    else
                      return v(true)
                    end
                  end)()
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
  local v = function(v1)
    return (function()
      if 2 == a then
        return (function()
          if Golden_TestValues_I_f(0) then
            return 10
          else
            return (function()
              if 3 == a then
                return (function()
                  local n1 = (function()
                    if 4 == a then
                      return (function()
                        local n2 = 0
                        local y = a
                        local z = a
                        return (function()
                          if Golden_TestValues_I_f(y) then
                            return z
                          else
                            return n2
                          end
                        end)()
                      end)()
                    else
                      return 0
                    end
                  end)()
                  local y = a
                  local z = a
                  return (function()
                    if Golden_TestValues_I_f(z) then return y else return n1 end
                  end)()
                end)()
              else
                return (function()
                  if 4 == a then
                    return (function()
                      local n3 = 0
                      local y = a
                      local z = a
                      return (function()
                        if Golden_TestValues_I_f(y) then
                          return z
                        else
                          return n3
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
          if 3 == a then
            return (function()
              local n4 = (function()
                if 4 == a then
                  return (function()
                    local n5 = 0
                    local y = a
                    local z = a
                    return (function()
                      if Golden_TestValues_I_f(y) then
                        return z
                      else
                        return n5
                      end
                    end)()
                  end)()
                else
                  return 0
                end
              end)()
              local y = a
              local z = a
              return (function()
                if Golden_TestValues_I_f(z) then return y else return n4 end
              end)()
            end)()
          else
            return (function()
              if 4 == a then
                return (function()
                  local n6 = 0
                  local y = a
                  local z = a
                  return (function()
                    if Golden_TestValues_I_f(y) then return z else return n6 end
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
  end
  return (function()
    if 1 == a then
      return (function()
        if "b" == b then
          return (function()
            local e7 = Golden_TestValues_I_f(2)
            return (function()
              if true == e7 then
                return (function()
                  local e8 = Golden_TestValues_I_f(1)
                  return (function()
                    if true == e8 then return 42 else return v(true) end
                  end)()
                end)()
              else
                return v(true)
              end
            end)()
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
return {
  Golden_TestCaseStatements_I_a = Golden_TestCaseStatements_I_a,
  Golden_TestCaseStatements_I_b = Golden_TestCaseStatements_I_b,
  Golden_TestCaseStatements_I_c = Golden_TestCaseStatements_I_c,
  Golden_TestCaseStatements_I_J = Golden_TestCaseStatements_I_J,
  Golden_TestCaseStatements_I_N = Golden_TestCaseStatements_I_N,
  Golden_TestCaseStatements_I_d = Golden_TestCaseStatements_I_d,
  Golden_TestCaseStatements_I_multipleGuards = Golden_TestCaseStatements_I_multipleGuards
}