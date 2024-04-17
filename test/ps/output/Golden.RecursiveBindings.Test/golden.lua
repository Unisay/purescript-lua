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
    a = b
    b = a
    local f = function(unused0) return a end
    local y = f(z)(z)
    return f(f(y)(y))(f(y)(0))
  end)()
}
