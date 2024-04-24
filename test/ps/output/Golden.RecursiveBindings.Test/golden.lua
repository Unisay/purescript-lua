return {
  letRec = (function()
    local no
    local yes
    no = function(v)
      if v then
        return yes(false)
      else
        if false == v then
          return yes(true)
        else
          return error("No patterns matched")
        end
      end
    end
    yes = function(v)
      if v then
        return no(false)
      else
        if false == v then
          return no(true)
        else
          return error("No patterns matched")
        end
      end
    end
    return no(false)
  end)(),
  whereRec = (function()
    local no
    local yes
    no = function(v)
      if v then
        return yes(false)
      else
        if false == v then
          return yes(true)
        else
          return error("No patterns matched")
        end
      end
    end
    yes = function(v)
      if v then
        return no(false)
      else
        if false == v then
          return no(true)
        else
          return error("No patterns matched")
        end
      end
    end
    return no(false)
  end)(),
  letRecMixed = (function()
    local z = 1
    local a
    local b
    a = function() return b(z) end
    b = function() return a(z) end
    local f = function() return a end
    local y = f(z)(z)
    return f(f(y)(y))(f(y)(0))
  end)()
}
