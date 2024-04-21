local M = {}
M.Golden_RecordsAccess_Test_r = { x = 1, y = true }
return {
  r = M.Golden_RecordsAccess_Test_r,
  test1 = M.Golden_RecordsAccess_Test_r.x,
  test2 = function(v) return v.x end,
  test3 = function(v) return v.x end,
  test4 = function(v) return v.x end
}
