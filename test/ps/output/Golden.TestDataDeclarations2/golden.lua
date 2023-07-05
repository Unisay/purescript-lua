return {
  CtorSameName = function()
    return { ["$ctor"] = "Golden_TestDataDeclarations2.CtorSameName" }
  end,
  test = function(v)
    return function(v1)
      if "Golden.TestDataDeclarations1.CtorSameName" == v["$ctor"] then
        return (function()
          if "Golden.TestDataDeclarations2.CtorSameName" == v1["$ctor"] then
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
}
