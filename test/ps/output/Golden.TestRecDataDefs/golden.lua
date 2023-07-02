local Golden_TestRecDataDefs_I_A = function()
  return { ["$ctor"] = "Golden_TestRecDataDefs.A" }
end
local Golden_TestRecDataDefs_I_AB = function(value0)
  return { ["$ctor"] = "Golden_TestRecDataDefs.AB", value0 = value0 }
end
local Golden_TestRecDataDefs_I_B = function()
  return { ["$ctor"] = "Golden_TestRecDataDefs.B" }
end
local Golden_TestRecDataDefs_I_BA = function(value0)
  return { ["$ctor"] = "Golden_TestRecDataDefs.BA", value0 = value0 }
end
local Golden_TestRecDataDefs_I_ab = Golden_TestRecDataDefs_I_AB(Golden_TestRecDataDefs_I_B)
return {
  A = Golden_TestRecDataDefs_I_A,
  AB = Golden_TestRecDataDefs_I_AB,
  B = Golden_TestRecDataDefs_I_B,
  BA = Golden_TestRecDataDefs_I_BA,
  a = Golden_TestRecDataDefs_I_A,
  b = Golden_TestRecDataDefs_I_B,
  ab = Golden_TestRecDataDefs_I_ab,
  ba = Golden_TestRecDataDefs_I_BA(Golden_TestRecDataDefs_I_ab)
}
