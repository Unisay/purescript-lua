return {
  U = { ["$ctor"] = "Unit.U" },
  P3 = function(value0)
    return function(value1)
      return function(value2)
        return {
          ["$ctor"] = "TProduct.P3",
          value0 = value0,
          value1 = value1,
          value2 = value2
        }
      end
    end
  end,
  PF = function(value0)
    return { ["$ctor"] = "TProductWithFields.PF", value0 = value0 }
  end,
  S0 = { ["$ctor"] = "TSum.S0" },
  S1 = function(value0) return { ["$ctor"] = "TSum.S1", value0 = value0 } end,
  S2 = function(value0)
    return function(value1)
      return { ["$ctor"] = "TSum.S2", value0 = value0, value1 = value1 }
    end
  end,
  Nop = { ["$ctor"] = "Rec.Nop" },
  More = function(value0)
    return { ["$ctor"] = "Rec.More", value0 = value0 }
  end,
  CtorSameName = { ["$ctor"] = "TySameName.CtorSameName" }
}
