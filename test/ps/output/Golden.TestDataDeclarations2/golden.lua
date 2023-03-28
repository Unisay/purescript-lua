local Golden_TestDataDeclarations1 = (function()
  local U = function()
    return { ["$ctor"] = "Golden_TestDataDeclarations1.U" }
  end
  local CtorSameName = function()
    return { ["$ctor"] = "Golden_TestDataDeclarations1.CtorSameName" }
  end
  local S0 = function()
    return { ["$ctor"] = "Golden_TestDataDeclarations1.S0" }
  end
  local S1 = function(value0)
    return { ["$ctor"] = "Golden_TestDataDeclarations1.S1", value0 = value0 }
  end
  local S2 = function(value0, value1)
    return {
      ["$ctor"] = "Golden_TestDataDeclarations1.S2",
      value0 = value0,
      value1 = value1
    }
  end
  local PF = function(value0)
    return { ["$ctor"] = "Golden_TestDataDeclarations1.PF", value0 = value0 }
  end
  local P3 = function(value0, value1, value2)
    return {
      ["$ctor"] = "Golden_TestDataDeclarations1.P3",
      value0 = value0,
      value1 = value1,
      value2 = value2
    }
  end
  local Nop = function()
    return { ["$ctor"] = "Golden_TestDataDeclarations1.Nop" }
  end
  local More = function(value0)
    return { ["$ctor"] = "Golden_TestDataDeclarations1.More", value0 = value0 }
  end
  return {
    U = U,
    P3 = P3,
    PF = PF,
    S0 = S0,
    S1 = S1,
    S2 = S2,
    Nop = Nop,
    More = More,
    CtorSameName = CtorSameName
  }
end)()
local Golden_TestDataDeclarations2 = (function()
  local CtorSameName = function()
    return { ["$ctor"] = "Golden_TestDataDeclarations2.CtorSameName" }
  end
  local test = function(v0)
    return function(v11)
      if "Golden.TestDataDeclarations1.CtorSameName" == v0["$ctor"] then
        return (function()
          if "Golden.TestDataDeclarations2.CtorSameName" == v11["$ctor"] then
            return true
          else
            return error("No patterns matched")
          end
        end)()
      else
        return error("No patterns matched")
      end
    end
  end
  return { CtorSameName = CtorSameName, test = test }
end)()
