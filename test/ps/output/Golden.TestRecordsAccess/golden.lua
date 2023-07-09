local Golden_TestRecordsAccess_I_r = { x = 1, y = true }
return {
  r = Golden_TestRecordsAccess_I_r,
  test1 = Golden_TestRecordsAccess_I_r.x,
  test2 = function(v) return v.x end,
  test3 = function(v) return v.x end,
  test4 = function(v) return v.x end
}
