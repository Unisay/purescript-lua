return {
  flip = function(f)
    return function(a) return function(b) return f(b)(a) end end
  end
}
