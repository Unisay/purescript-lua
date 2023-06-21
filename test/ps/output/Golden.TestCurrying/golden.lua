local Golden_TestCurrying_I_f = function()
  return function() return function() return function() return "ok" end end end
end
local Golden_TestCurrying_I_apply = function(f1)
  return function(x) return f1(x) end
end
