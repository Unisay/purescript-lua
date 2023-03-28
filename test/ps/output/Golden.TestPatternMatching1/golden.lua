local Golden_TestPatternMatching1 = (function()
  local Zero = function()
    return { ["$ctor"] = "Golden_TestPatternMatching1.Zero" }
  end
  local Succ = function(value0)
    return { ["$ctor"] = "Golden_TestPatternMatching1.Succ", value0 = value0 }
  end
  local Num = function(value0)
    return { ["$ctor"] = "Golden_TestPatternMatching1.Num", value0 = value0 }
  end
  local Not = function(value0)
    return { ["$ctor"] = "Golden_TestPatternMatching1.Not", value0 = value0 }
  end
  local pat = function(e0)
    if "Golden.TestPatternMatching1.Not" == e0["$ctor"] then
      return (function()
        if "Golden.TestPatternMatching1.Num" == e0[0]["$ctor"] then
          return (function()
            if "Golden.TestPatternMatching1.Succ" == e0[0][0]["$ctor"] then
              return 1
            else
              return (function()
                if "Golden.TestPatternMatching1.Zero" == e0[0][0]["$ctor"] then
                  return 2
                else
                  return 6
                end
              end)()
            end
          end)()
        else
          return (function()
            if "Golden.TestPatternMatching1.Not" == e0[0]["$ctor"] then
              return (function()
                if "Golden.TestPatternMatching1.Num" == e0[0][0]["$ctor"] then
                  return (function()
                    if "Golden.TestPatternMatching1.Succ" == e0[0][0][0]["$ctor"] then
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
        if "Golden.TestPatternMatching1.Num" == e0["$ctor"] then
          return (function()
            if "Golden.TestPatternMatching1.Succ" == e0[0]["$ctor"] then
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
  end
  return { Zero = Zero, Succ = Succ, Num = Num, Not = Not, pat = pat }
end)()
