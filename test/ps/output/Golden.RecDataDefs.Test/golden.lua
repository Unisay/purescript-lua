PSLUA_Golden_RecDataDefs_Test_A = { ["$ctor"] = "Golden.RecDataDefs.Test∷A.A" }
PSLUA_Golden_RecDataDefs_Test_AB = function(value0)
  return { ["$ctor"] = "Golden.RecDataDefs.Test∷A.AB", value0 = value0 }
end
PSLUA_Golden_RecDataDefs_Test_B = { ["$ctor"] = "Golden.RecDataDefs.Test∷B.B" }
PSLUA_Golden_RecDataDefs_Test_BA = function(value0)
  return { ["$ctor"] = "Golden.RecDataDefs.Test∷B.BA", value0 = value0 }
end
PSLUA_Golden_RecDataDefs_Test_ab = PSLUA_Golden_RecDataDefs_Test_AB(PSLUA_Golden_RecDataDefs_Test_B)
return {
  A = PSLUA_Golden_RecDataDefs_Test_A,
  AB = PSLUA_Golden_RecDataDefs_Test_AB,
  B = PSLUA_Golden_RecDataDefs_Test_B,
  BA = PSLUA_Golden_RecDataDefs_Test_BA,
  a = PSLUA_Golden_RecDataDefs_Test_A,
  b = PSLUA_Golden_RecDataDefs_Test_B,
  ab = PSLUA_Golden_RecDataDefs_Test_ab,
  ba = PSLUA_Golden_RecDataDefs_Test_BA(PSLUA_Golden_RecDataDefs_Test_ab)
}
