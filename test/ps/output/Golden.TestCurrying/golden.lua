local Golden_TestCurrying_I_f = function(i0)
  return function(b1)
    return function(c2) return function(d3) return "ok" end end
  end
end
local Golden_TestCurrying_I_apply = function(f14)
  return function(x5) return f14(x5) end
end
