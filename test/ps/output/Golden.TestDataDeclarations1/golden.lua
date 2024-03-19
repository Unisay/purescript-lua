return {
  U = { ["$ctor"] = "Golden.TestDataDeclarations1∷Unit.U" },
  P3 = function(value0)
    return function(value1)
      return function(value2)
        return {
          ["$ctor"] = "Golden.TestDataDeclarations1∷TProduct.P3",
          value0 = value0,
          value1 = value1,
          value2 = value2
        }
      end
    end
  end,
  PF = function(value0)
    return {
      ["$ctor"] = "Golden.TestDataDeclarations1∷TProductWithFields.PF",
      value0 = value0
    }
  end,
  S0 = { ["$ctor"] = "Golden.TestDataDeclarations1∷TSum.S0" },
  S1 = function(value0)
    return {
      ["$ctor"] = "Golden.TestDataDeclarations1∷TSum.S1",
      value0 = value0
    }
  end,
  S2 = function(value0)
    return function(value1)
      return {
        ["$ctor"] = "Golden.TestDataDeclarations1∷TSum.S2",
        value0 = value0,
        value1 = value1
      }
    end
  end,
  Nop = { ["$ctor"] = "Golden.TestDataDeclarations1∷Rec.Nop" },
  More = function(value0)
    return {
      ["$ctor"] = "Golden.TestDataDeclarations1∷Rec.More",
      value0 = value0
    }
  end,
  CtorSameName = {
    ["$ctor"] = "Golden.TestDataDeclarations1∷TySameName.CtorSameName"
  }
}
