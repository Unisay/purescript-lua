return {
  CtorSameName = { ["$ctor"] = "TySameName.CtorSameName" },
  test = function(v)
    return function(v1)
      if "TySameName.CtorSameName" == v["$ctor"] then
        return (function()
          if "TySameName.CtorSameName" == v1["$ctor"] then
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
