return {
  Zero = { ["$ctor"] = "N.Zero" },
  Succ = function(value0) return { ["$ctor"] = "N.Succ", value0 = value0 } end,
  Num = function(value0) return { ["$ctor"] = "E.Num", value0 = value0 } end,
  Not = function(value0) return { ["$ctor"] = "E.Not", value0 = value0 } end,
  pat = function(e)
    if "E.Not" == e["$ctor"] then
      return (function()
        if "E.Num" == e.value0["$ctor"] then
          return (function()
            if "N.Succ" == e.value0.value0["$ctor"] then
              return 1
            else
              return (function()
                if "N.Zero" == e.value0.value0["$ctor"] then
                  return 2
                else
                  return 6
                end
              end)()
            end
          end)()
        else
          return (function()
            if "E.Not" == e.value0["$ctor"] then
              return (function()
                if "E.Num" == e.value0.value0["$ctor"] then
                  return (function()
                    if "N.Succ" == e.value0.value0.value0["$ctor"] then
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
        if "E.Num" == e["$ctor"] then
          return (function()
            if "N.Succ" == e.value0["$ctor"] then return 4 else return 5 end
          end)()
        else
          return 6
        end
      end)()
    end
  end,
  T = function(value0)
    return function(value1)
      return { ["$ctor"] = "Tuple.T", value0 = value0, value1 = value1 }
    end
  end,
  fst = function(v) return v.value0 end,
  snd = function(v) return v.value1 end
}
