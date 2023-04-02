local Golden_TestRecursiveBindings_I_whereRec = (function()
  local yes0
  local no1
  yes0 = function(v2)
    if true == v2 then
      return no1(false)
    else
      return (function()
        if false == v2 then
          return no1(true)
        else
          return error("No patterns matched")
        end
      end)()
    end
  end
  no1 = function(v3)
    if true == v3 then
      return yes0(false)
    else
      return (function()
        if false == v3 then
          return yes0(true)
        else
          return error("No patterns matched")
        end
      end)()
    end
  end
  return no1(false)
end)()
local Golden_TestRecursiveBindings_I_letRecMixed = (function()
  local z4 = 1
  local b5
  local a6
  b5 = function(v10) return a6(z4) end
  a6 = function(v11) return b5(z4) end
  local f7 = function(v12) return function(k13) return a6(k13) end end
  local y8 = f7(z4)(z4)
  local x9 = f7(y8)(y8)
  return f7(x9)(f7(y8)(0))
end)()
local Golden_TestRecursiveBindings_I_letRec = (function()
  local yes14
  local no15
  yes14 = function(v16)
    if true == v16 then
      return no15(false)
    else
      return (function()
        if false == v16 then
          return no15(true)
        else
          return error("No patterns matched")
        end
      end)()
    end
  end
  no15 = function(v17)
    if true == v17 then
      return yes14(false)
    else
      return (function()
        if false == v17 then
          return yes14(true)
        else
          return error("No patterns matched")
        end
      end)()
    end
  end
  return no15(false)
end)()
