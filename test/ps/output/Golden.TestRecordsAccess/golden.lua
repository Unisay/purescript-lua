local Golden_TestRecordsAccess = (function()
  local test4 = function(v0) local x1 = v0.x return x1 end
  local test3 = function(v2) local x3 = v2.x return x3 end
  local test2 = function(v4) return v4.x end
  local r = { x = 1, y = true }
  local test1 = r.x
  return { r = r, test1 = test1, test2 = test2, test3 = test3, test4 = test4 }
end)()
