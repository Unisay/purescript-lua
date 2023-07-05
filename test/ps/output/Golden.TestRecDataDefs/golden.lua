return {
  A = function() return { ["$ctor"] = "Golden_TestRecDataDefs.A" } end,
  AB = function(value0)
    return { ["$ctor"] = "Golden_TestRecDataDefs.AB", value0 = value0 }
  end,
  B = function() return { ["$ctor"] = "Golden_TestRecDataDefs.B" } end,
  BA = function(value0)
    return { ["$ctor"] = "Golden_TestRecDataDefs.BA", value0 = value0 }
  end,
  a = function() return { ["$ctor"] = "Golden_TestRecDataDefs.A" } end,
  b = function() return { ["$ctor"] = "Golden_TestRecDataDefs.B" } end,
  ab = (function(value0)
    return { ["$ctor"] = "Golden_TestRecDataDefs.AB", value0 = value0 }
  end)(function() return { ["$ctor"] = "Golden_TestRecDataDefs.B" } end),
  ba = (function(value0)
    return { ["$ctor"] = "Golden_TestRecDataDefs.BA", value0 = value0 }
  end)((function(value0)
    return { ["$ctor"] = "Golden_TestRecDataDefs.AB", value0 = value0 }
  end)(function() return { ["$ctor"] = "Golden_TestRecDataDefs.B" } end))
}
