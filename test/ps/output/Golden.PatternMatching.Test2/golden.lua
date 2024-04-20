local PSLUA_Golden_PatternMatching_Test2_bat
PSLUA_Golden_PatternMatching_Test2_bat = function(n)
  if "Golden.PatternMatching.Test1∷N.Zero" == n["$ctor"] then
    return 1
  else
    if "Golden.PatternMatching.Test1∷N.Succ" == n["$ctor"] then
      return PSLUA_Golden_PatternMatching_Test2_bat(n.value0)
    else
      return error("No patterns matched")
    end
  end
end
return {
  Zero = { ["$ctor"] = "Golden.PatternMatching.Test2∷N.Zero" },
  Succ = function(value0)
    return {
      ["$ctor"] = "Golden.PatternMatching.Test2∷N.Succ",
      value0 = value0
    }
  end,
  Add = function(value0)
    return function(value1)
      return {
        ["$ctor"] = "Golden.PatternMatching.Test2∷N.Add",
        value0 = value0,
        value1 = value1
      }
    end
  end,
  Mul = function(value0)
    return function(value1)
      return {
        ["$ctor"] = "Golden.PatternMatching.Test2∷N.Mul",
        value0 = value0,
        value1 = value1
      }
    end
  end,
  pat = function(e)
    if "Golden.PatternMatching.Test2∷N.Add" == e["$ctor"] then
      if "Golden.PatternMatching.Test2∷N.Zero" == e.value1["$ctor"] then
        if "Golden.PatternMatching.Test2∷N.Add" == e.value0["$ctor"] then
          return 1
        else
          if "Golden.PatternMatching.Test2∷N.Mul" == e.value0["$ctor"] then
            return 2
          else
            return 5
          end
        end
      else
        if "Golden.PatternMatching.Test2∷N.Mul" == e.value1["$ctor"] then
          return 3
        else
          if "Golden.PatternMatching.Test2∷N.Add" == e.value1["$ctor"] then
            return 4
          else
            if "Golden.PatternMatching.Test2∷N.Zero" == e.value1["$ctor"] then
              return 5
            else
              return 6
            end
          end
        end
      end
    else
      return 6
    end
  end,
  bat = PSLUA_Golden_PatternMatching_Test2_bat
}
