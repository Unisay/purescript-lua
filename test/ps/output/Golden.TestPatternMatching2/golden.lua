return {
  Zero = function()
    return { ["$ctor"] = "Golden_TestPatternMatching2.Zero" }
  end,
  Succ = function(value0)
    return { ["$ctor"] = "Golden_TestPatternMatching2.Succ", value0 = value0 }
  end,
  Add = function(value0, value1)
    return {
      ["$ctor"] = "Golden_TestPatternMatching2.Add",
      value0 = value0,
      value1 = value1
    }
  end,
  Mul = function(value0, value1)
    return {
      ["$ctor"] = "Golden_TestPatternMatching2.Mul",
      value0 = value0,
      value1 = value1
    }
  end,
  pat = function(e)
    if "Golden.TestPatternMatching2.Add" == e["$ctor"] then
      return (function()
        if "Golden.TestPatternMatching2.Zero" == e[1]["$ctor"] then
          return (function()
            if "Golden.TestPatternMatching2.Add" == e[0]["$ctor"] then
              return 1
            else
              return (function()
                if "Golden.TestPatternMatching2.Mul" == e[0]["$ctor"] then
                  return 2
                else
                  return 5
                end
              end)()
            end
          end)()
        else
          return (function()
            if "Golden.TestPatternMatching2.Mul" == e[1]["$ctor"] then
              return 3
            else
              return (function()
                if "Golden.TestPatternMatching2.Add" == e[1]["$ctor"] then
                  return 4
                else
                  return (function()
                    if "Golden.TestPatternMatching2.Zero" == e[1]["$ctor"] then
                      return 5
                    else
                      return 6
                    end
                  end)()
                end
              end)()
            end
          end)()
        end
      end)()
    else
      return 6
    end
  end
}
