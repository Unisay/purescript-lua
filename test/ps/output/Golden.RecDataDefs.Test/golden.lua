M = {}
M.PSLUA_Golden_RecDataDefs_Test_A = {
  ["$ctor"] = "Golden.RecDataDefs.Test∷A.A"
}
M.PSLUA_Golden_RecDataDefs_Test_AB = function(value0)
  return { ["$ctor"] = "Golden.RecDataDefs.Test∷A.AB", value0 = value0 }
end
M.PSLUA_Golden_RecDataDefs_Test_B = {
  ["$ctor"] = "Golden.RecDataDefs.Test∷B.B"
}
M.PSLUA_Golden_RecDataDefs_Test_BA = function(value0)
  return { ["$ctor"] = "Golden.RecDataDefs.Test∷B.BA", value0 = value0 }
end
M.PSLUA_Golden_RecDataDefs_Test_ab = M.PSLUA_Golden_RecDataDefs_Test_AB(M.PSLUA_Golden_RecDataDefs_Test_B)
return {
  A = M.PSLUA_Golden_RecDataDefs_Test_A,
  AB = M.PSLUA_Golden_RecDataDefs_Test_AB,
  B = M.PSLUA_Golden_RecDataDefs_Test_B,
  BA = M.PSLUA_Golden_RecDataDefs_Test_BA,
  a = M.PSLUA_Golden_RecDataDefs_Test_A,
  b = M.PSLUA_Golden_RecDataDefs_Test_B,
  ab = M.PSLUA_Golden_RecDataDefs_Test_ab,
  ba = M.PSLUA_Golden_RecDataDefs_Test_BA(M.PSLUA_Golden_RecDataDefs_Test_ab)
}
