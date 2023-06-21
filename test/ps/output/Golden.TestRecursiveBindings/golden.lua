local Golden_TestRecursiveBindings_I_whereRec = (function()
  local yes
  local no
  yes = function(v)
    return (function()
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
    end)()
  end
  no = function(v)
    return (function()
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
    end)()
  end
  return no(false)
end)()
local Golden_TestRecursiveBindings_I_letRecMixed = (function()
  local z = 1
  local b
  local a
  b = function() return a(z) end
  a = function() return b(z) end
  local f = function() return function(k) return a(k) end end
  local y = f(z)(z)
  return f(f(y)(y))(f(y)(0))
end)()
local Golden_TestRecursiveBindings_I_letRec = (function()
  local yes
  local no
  yes = function(v)
    return (function()
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
    end)()
  end
  no = function(v)
    return (function()
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
    end)()
  end
  return no(false)
end)()
