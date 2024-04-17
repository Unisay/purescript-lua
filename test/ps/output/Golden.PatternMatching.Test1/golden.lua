return {
  Zero = { ["$ctor"] = "Golden.PatternMatching.Test1∷N.Zero" },
  Succ = function(value0)
    return {
      ["$ctor"] = "Golden.PatternMatching.Test1∷N.Succ",
      value0 = value0
    }
  end,
  Num = function(value0)
    return { ["$ctor"] = "Golden.PatternMatching.Test1∷E.Num", value0 = value0 }
  end,
  Not = function(value0)
    return { ["$ctor"] = "Golden.PatternMatching.Test1∷E.Not", value0 = value0 }
  end,
  pat = function(e)
    if "Golden.PatternMatching.Test1∷E.Not" == e["$ctor"] then
      if "Golden.PatternMatching.Test1∷E.Num" == e.value0["$ctor"] then
        if "Golden.PatternMatching.Test1∷N.Succ" == e.value0.value0["$ctor"] then
          return 1
        else
          if "Golden.PatternMatching.Test1∷N.Zero" == e.value0.value0["$ctor"] then
            return 2
          else
            return 6
          end
        end
      else
        if "Golden.PatternMatching.Test1∷E.Not" == e.value0["$ctor"] then
          if "Golden.PatternMatching.Test1∷E.Num" == e.value0.value0["$ctor"] then
            if "Golden.PatternMatching.Test1∷N.Succ" == e.value0.value0.value0["$ctor"] then
              return 3
            else
              return 6
            end
          else
            return 6
          end
        else
          return 6
        end
      end
    else
      if "Golden.PatternMatching.Test1∷E.Num" == e["$ctor"] then
        if "Golden.PatternMatching.Test1∷N.Succ" == e.value0["$ctor"] then
          return 4
        else
          return 5
        end
      else
        return 6
      end
    end
  end,
  T = function(value0)
    return function(value1)
      return {
        ["$ctor"] = "Golden.PatternMatching.Test1∷Tuple.T",
        value0 = value0,
        value1 = value1
      }
    end
  end,
  fst = function(v) return v.value0 end,
  snd = function(v) return v.value1 end
}
