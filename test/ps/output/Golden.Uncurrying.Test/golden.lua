local M = {}
M.Golden_Uncurrying_Test_uncurryFirst2Args = function(i)
  return function() return i end
end
return {
  call2 = M.Golden_Uncurrying_Test_uncurryFirst2Args(1, true),
  call3 = M.Golden_Uncurrying_Test_uncurryFirst2Args(2, false)("a"),
  call4 = function() return 1 end,
  call5 = function() return 3 end
}
