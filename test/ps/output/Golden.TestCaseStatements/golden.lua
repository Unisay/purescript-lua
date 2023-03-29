local Golden_TestValues = (function()
  local f = function() return true end
  return { f = f }
end)()
local Golden_TestCaseStatements = (function()
  local J = function(value0)
    return { ["$ctor"] = "Golden_TestCaseStatements.J", value0 = value0 }
  end
  local N = function() return { ["$ctor"] = "Golden_TestCaseStatements.N" } end
  local multipleGuards = 1
  local d = function(m0)
    return function(n1)
      return function(x2)
        local v3 = function()
          return (function() if "y" == x2 then return 0 else return 1 end end)()
        end
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
  local b = "b"
  local a = 1
  local c = (function()
    local v5 = function()
      return (function()
        if 2 == a then
          return (function()
            if Golden_TestValues.f(0) then
              return 10
            else
              return (function()
                if 3 == a then
                  return (function()
                    local n18 = (function()
                      if 4 == a then
                        return (function()
                          local n211 = 0
                          local y12 = a
                          local z13 = a
                          return (function()
                            if Golden_TestValues.f(y12) then
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
                    local y9 = a
                    local z10 = a
                    return (function()
                      if Golden_TestValues.f(z10) then
                        return y9
                      else
                        return n18
                      end
                    end)()
                  end)()
                else
                  return (function()
                    if 4 == a then
                      return (function()
                        local n314 = 0
                        local y15 = a
                        local z16 = a
                        return (function()
                          if Golden_TestValues.f(y15) then
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
            if 3 == a then
              return (function()
                local n417 = (function()
                  if 4 == a then
                    return (function()
                      local n520 = 0
                      local y21 = a
                      local z22 = a
                      return (function()
                        if Golden_TestValues.f(y21) then
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
                local y18 = a
                local z19 = a
                return (function()
                  if Golden_TestValues.f(z19) then
                    return y18
                  else
                    return n417
                  end
                end)()
              end)()
            else
              return (function()
                if 4 == a then
                  return (function()
                    local n623 = 0
                    local y24 = a
                    local z25 = a
                    return (function()
                      if Golden_TestValues.f(y24) then
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
      end)()
    end
    return (function()
      if 1 == a then
        return (function()
          if "b" == b then
            return (function()
              local e76 = Golden_TestValues.f(2)
              return (function()
                if true == e76 then
                  return (function()
                    local e87 = Golden_TestValues.f(1)
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
  return {
    a = a,
    b = b,
    c = c,
    J = J,
    N = N,
    d = d,
    multipleGuards = multipleGuards
  }
end)()
