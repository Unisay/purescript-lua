local M = {}
M.Golden_Uncurrying_Test_f = function(i)
  return function() return function() return i end end
end
return {
  f = M.Golden_Uncurrying_Test_f,
  call2 = M.Golden_Uncurrying_Test_f(1)(true),
  call3 = M.Golden_Uncurrying_Test_f(2)(false)("a")
}
