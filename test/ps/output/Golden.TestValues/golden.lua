local Golden_TestValues = (function()
  local i = 1
  local f = function() return true end
  local c = "c"
  local b = true
  local o = { i = i, b = b, c = c }
  local a = { 1, 2, 3 }
  return { i = i, b = b, c = c, a = a, o = o, f = f }
end)()
