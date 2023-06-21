local Golden_TestPatternMatching1_I_Zero = function()
  return { ["$ctor"] = "Golden_TestPatternMatching1.Zero" }
end
local Golden_TestPatternMatching1_I_Succ = function(value0)
  return { ["$ctor"] = "Golden_TestPatternMatching1.Succ", value0 = value0 }
end
local Golden_TestPatternMatching1_I_Num = function(value0)
  return { ["$ctor"] = "Golden_TestPatternMatching1.Num", value0 = value0 }
end
local Golden_TestPatternMatching1_I_Not = function(value0)
  return { ["$ctor"] = "Golden_TestPatternMatching1.Not", value0 = value0 }
end
local Golden_TestPatternMatching1_I_pat = function(e)
  return (function()
    if "Golden.TestPatternMatching1.Not" == e["$ctor"] then
      return (function()
        if "Golden.TestPatternMatching1.Num" == e[0]["$ctor"] then
          return (function()
            if "Golden.TestPatternMatching1.Succ" == e[0][0]["$ctor"] then
              return 1
            else
              return (function()
                if "Golden.TestPatternMatching1.Zero" == e[0][0]["$ctor"] then
                  return 2
                else
                  return 6
                end
              end)()
            end
          end)()
        else
          return (function()
            if "Golden.TestPatternMatching1.Not" == e[0]["$ctor"] then
              return (function()
                if "Golden.TestPatternMatching1.Num" == e[0][0]["$ctor"] then
                  return (function()
                    if "Golden.TestPatternMatching1.Succ" == e[0][0][0]["$ctor"] then
                      return 3
                    else
                      return 6
                    end
                  end)()
                else
                  return 6
                end
              end)()
            else
              return 6
            end
          end)()
        end
      end)()
    else
      return (function()
        if "Golden.TestPatternMatching1.Num" == e["$ctor"] then
          return (function()
            if "Golden.TestPatternMatching1.Succ" == e[0]["$ctor"] then
              return 4
            else
              return 5
            end
          end)()
        else
          return 6
        end
      end)()
    end
  end)()
end
