local Golden_TestDataDeclarations2_I_CtorSameName = function()
  return { ["$ctor"] = "Golden_TestDataDeclarations2.CtorSameName" }
end
local Golden_TestDataDeclarations2_I_test = function(v0)
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
