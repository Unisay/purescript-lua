local M = {}
M.Data_Maybe_Nothing = { ["$ctor"] = "Data.Maybe∷Maybe.Nothing" }
M.Data_Maybe_Just = function(value0)
  return { ["$ctor"] = "Data.Maybe∷Maybe.Just", value0 = value0 }
end
M.Data_Maybe_maybe = function(v)
  return function(v1)
    return function(v2)
      if "Data.Maybe∷Maybe.Nothing" == v2["$ctor"] then
        return v
      else
        if "Data.Maybe∷Maybe.Just" == v2["$ctor"] then
          return v1(v2.value0)
        else
          return error("No patterns matched")
        end
      end
    end
  end
end
M.Golden_MaybeChainModule_Test_identity = function(x) return x end
M.Golden_MaybeChainModule_Test_map = function(v)
  return function(v1)
    if "Data.Maybe∷Maybe.Just" == v1["$ctor"] then
      return M.Data_Maybe_Just(v(v1.value0))
    else
      return M.Data_Maybe_Nothing
    end
  end
end
return {
  chainedNothing = M.Data_Maybe_maybe(0)(M.Golden_MaybeChainModule_Test_identity)(M.Data_Maybe_maybe(M.Data_Maybe_Nothing)(M.Data_Maybe_Just)(M.Golden_MaybeChainModule_Test_map(function( x )
    return x
  end)(M.Data_Maybe_Nothing))),
  chainedJust = M.Data_Maybe_maybe(0)(M.Golden_MaybeChainModule_Test_identity)(M.Data_Maybe_maybe(M.Data_Maybe_Nothing)(M.Data_Maybe_Just)(M.Golden_MaybeChainModule_Test_map(function( x )
    return x
  end)(M.Data_Maybe_Just(42))))
}
