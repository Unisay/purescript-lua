local Golden_TestNewtype = (function()
  local NT = function(x0) return x0 end
  local g = NT
  local f = function(v1) local n2 = v1 return n2.foo end
  return { NT = NT, f = f, g = g }
end)()
