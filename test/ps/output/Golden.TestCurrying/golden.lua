local Golden_TestCurrying = (function()
  local f = function()
    return function()
      return function() return function() return "ok" end end
    end
  end
  local apply = function(f10) return function(x1) return f10(x1) end end
  return { apply = apply, f = f }
end)()
