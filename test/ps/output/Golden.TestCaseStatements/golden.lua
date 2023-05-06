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
                return (function()
                  if "Golden.TestCaseStatements.N" == n1["$ctor"] then
                    return m0[0]
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
local Golden_TestCaseStatements_I_c = (function()
  local v6 = function(v19)
    if 2 == 1 then
      return (function()
        if (function(v0) return true end)(0) then
          return 10
        else
          return (function()
            if 3 == 1 then
              return (function()
                return (function()
                  if (function(v0) return true end)(1) then
                    return 1
                  else
                    return (function()
                      if 4 == 1 then
                        return (function()
                          return (function()
                            if (function(v0) return true end)(1) then
                              return 1
                            else
                              return 0
                            end
                          end)()
                        end)()
                      else
                        return 0
                      end
                    end)()
                  end
                end)()
              end)()
            else
              return (function()
                if 4 == 1 then
                  return (function()
                    return (function()
                      if (function(v0) return true end)(1) then
                        return 1
                      else
                        return 0
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
        if 3 == 1 then
          return (function()
            return (function()
              if (function(v0) return true end)(1) then
                return 1
              else
                return (function()
                  if 4 == 1 then
                    return (function()
                      return (function()
                        if (function(v0) return true end)(1) then
                          return 1
                        else
                          return 0
                        end
                      end)()
                    end)()
                  else
                    return 0
                  end
                end)()
              end
            end)()
          end)()
        else
          return (function()
            if 4 == 1 then
              return (function()
                return (function()
                  if (function(v0) return true end)(1) then
                    return 1
                  else
                    return 0
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
    if 1 == 1 then
      return (function()
        if "b" == "b" then
          return (function()
            return (function()
              if true == (function(v0) return true end)(2) then
                return (function()
                  return (function()
                    if true == (function(v0) return true end)(1) then
                      return 42
                    else
                      return v6(true)
                    end
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
