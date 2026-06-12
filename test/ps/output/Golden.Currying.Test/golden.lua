return {
  apply = function(f1) return function(x) return f1(x) end end,
  f = function()
    return function()
      return function() return function() return "ok" end end
    end
  end
}
