local M = {}
M.Golden_Unbinding_Test_f = function() return function() return 3 end end
return {
  a = 1,
  b = 2,
  f = M.Golden_Unbinding_Test_f,
  c = M.Golden_Unbinding_Test_f(1)(M.Golden_Unbinding_Test_f(2)(1))
}
