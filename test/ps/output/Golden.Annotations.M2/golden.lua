return {
  inlineIntoMe = function(i)
    return (function(v)
      if 1 == v then return 2 else return v end
    end)((function(v)
      if 1 == v then return 2 else return v end
    end)((function(v) if 1 == v then return 2 else return v end end)(i)))
  end
}
