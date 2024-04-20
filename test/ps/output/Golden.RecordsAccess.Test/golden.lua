PSLUA_Golden_RecordsAccess_Test_r = { x = 1, y = true }
return {
  r = PSLUA_Golden_RecordsAccess_Test_r,
  test1 = PSLUA_Golden_RecordsAccess_Test_r.x,
  test2 = function(v) return v.x end,
  test3 = function(v) return v.x end,
  test4 = function(v) return v.x end
}
