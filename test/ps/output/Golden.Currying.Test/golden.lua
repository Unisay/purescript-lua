return {
  apply = function(f1) return f1 end,
  f = function()
    return function()
      return function() return function() return "ok" end end
    end
  end
}
