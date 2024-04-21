local M = {}
M.Golden_RecDataDefs_Test_A = { ["$ctor"] = "Golden.RecDataDefs.Test∷A.A" }
M.Golden_RecDataDefs_Test_AB = function(value0)
  return { ["$ctor"] = "Golden.RecDataDefs.Test∷A.AB", value0 = value0 }
end
M.Golden_RecDataDefs_Test_B = { ["$ctor"] = "Golden.RecDataDefs.Test∷B.B" }
M.Golden_RecDataDefs_Test_BA = function(value0)
  return { ["$ctor"] = "Golden.RecDataDefs.Test∷B.BA", value0 = value0 }
end
M.Golden_RecDataDefs_Test_ab = M.Golden_RecDataDefs_Test_AB(M.Golden_RecDataDefs_Test_B)
return {
  A = M.Golden_RecDataDefs_Test_A,
  AB = M.Golden_RecDataDefs_Test_AB,
  B = M.Golden_RecDataDefs_Test_B,
  BA = M.Golden_RecDataDefs_Test_BA,
  a = M.Golden_RecDataDefs_Test_A,
  b = M.Golden_RecDataDefs_Test_B,
  ab = M.Golden_RecDataDefs_Test_ab,
  ba = M.Golden_RecDataDefs_Test_BA(M.Golden_RecDataDefs_Test_ab)
}
