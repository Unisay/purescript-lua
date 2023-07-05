return {
  letRec = (function()
    local yes
    local no
    yes = function(v)
      if true == v then
        return no(false)
      else
        return (function()
          if false == v then
            return no(true)
          else
            return error("No patterns matched")
          end
        end)()
      end
    end
    no = function(v)
      if true == v then
        return yes(false)
      else
        return (function()
          if false == v then
            return yes(true)
          else
            return error("No patterns matched")
          end
        end)()
      end
    end
    return no(false)
  end)(),
  whereRec = (function()
    local yes
    local no
    yes = function(v)
      if true == v then
        return no(false)
      else
        return (function()
          if false == v then
            return no(true)
          else
            return error("No patterns matched")
          end
        end)()
      end
    end
    no = function(v)
      if true == v then
        return yes(false)
      else
        return (function()
          if false == v then
            return yes(true)
          else
            return error("No patterns matched")
          end
        end)()
      end
    end
    return no(false)
  end)(),
  letRecMixed = (function()
    local z = 1
    local b
    local a
    b = function(unused0) return a(z) end
    a = function(unused1) return b(z) end
    local f = function(unused2) return function(k) return a(k) end end
    local y = f(z)(z)
    return f(f(y)(y))(f(y)(0))
  end)()
}
