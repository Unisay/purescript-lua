return {
  a = 1,
  b = 2,
  f = function(unused1) return function(unused0) return 3 end end,
  c = (function(unused1)
    return function(unused0) return 3 end
  end)(1)((function(unused1) return function(unused0) return 3 end end)(2)(1))
}
