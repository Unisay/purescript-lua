return {
  Zero = { ["$ctor"] = "Golden.TestPatternMatching2∷N.Zero" },
  Succ = function(value0)
    return { ["$ctor"] = "Golden.TestPatternMatching2∷N.Succ", value0 = value0 }
  end,
  Add = function(value0)
    return function(value1)
      return {
        ["$ctor"] = "Golden.TestPatternMatching2∷N.Add",
        value0 = value0,
        value1 = value1
      }
    end
  end,
  Mul = function(value0)
    return function(value1)
      return {
        ["$ctor"] = "Golden.TestPatternMatching2∷N.Mul",
        value0 = value0,
        value1 = value1
      }
    end
  end,
  pat = function(e)
    if "Golden.TestPatternMatching2∷N.Add" == e["$ctor"] then
      return (function()
        if "Golden.TestPatternMatching2∷N.Zero" == e.value1["$ctor"] then
          return (function()
            if "Golden.TestPatternMatching2∷N.Add" == e.value0["$ctor"] then
              return 1
            else
              return (function()
                if "Golden.TestPatternMatching2∷N.Mul" == e.value0["$ctor"] then
                  return 2
                else
                  return 5
                end
              end)()
            end
          end)()
        else
          return (function()
            if "Golden.TestPatternMatching2∷N.Mul" == e.value1["$ctor"] then
              return 3
            else
              return (function()
                if "Golden.TestPatternMatching2∷N.Add" == e.value1["$ctor"] then
                  return 4
                else
                  return (function()
                    if "Golden.TestPatternMatching2∷N.Zero" == e.value1["$ctor"] then
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
