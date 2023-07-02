return {
  apply = function(f1) return function(x) return f1(x) end end,
  f = function(unused3)
    return function(unused2)
      return function(unused1) return function(unused0) return "ok" end end
    end
  end
}
