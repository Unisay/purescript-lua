local Golden_TestUnbinding = (function()
  local f = function() return function() return 3 end end
  local b = 2
  local a = 1
  local c = f(a)(f(b)(a))
  return { a = a, b = b, f = f, c = c }
end)()
