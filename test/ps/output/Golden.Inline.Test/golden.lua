local M = {}
M.Golden_Inline_Test_runMu = function(v) return v(v) end
return {
  main = 1,
  runMu = M.Golden_Inline_Test_runMu,
  iMu = M.Golden_Inline_Test_runMu
}
