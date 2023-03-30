local Golden_TestCurrying_I_f = function()
  return function() return function() return function() return "ok" end end end
end
local Golden_TestCurrying_I_apply = function(f10)
  return function(x1) return f10(x1) end
end
