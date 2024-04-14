local Golden_RecDataDefs_Test_I_A = {
  ["$ctor"] = "Golden.RecDataDefs.Test∷A.A"
}
local Golden_RecDataDefs_Test_I_AB = function(value0)
  return { ["$ctor"] = "Golden.RecDataDefs.Test∷A.AB", value0 = value0 }
end
local Golden_RecDataDefs_Test_I_B = {
  ["$ctor"] = "Golden.RecDataDefs.Test∷B.B"
}
local Golden_RecDataDefs_Test_I_BA = function(value0)
  return { ["$ctor"] = "Golden.RecDataDefs.Test∷B.BA", value0 = value0 }
end
local Golden_RecDataDefs_Test_I_ab = Golden_RecDataDefs_Test_I_AB(Golden_RecDataDefs_Test_I_B)
return {
  A = Golden_RecDataDefs_Test_I_A,
  AB = Golden_RecDataDefs_Test_I_AB,
  B = Golden_RecDataDefs_Test_I_B,
  BA = Golden_RecDataDefs_Test_I_BA,
  a = Golden_RecDataDefs_Test_I_A,
  b = Golden_RecDataDefs_Test_I_B,
  ab = Golden_RecDataDefs_Test_I_ab,
  ba = Golden_RecDataDefs_Test_I_BA(Golden_RecDataDefs_Test_I_ab)
}
