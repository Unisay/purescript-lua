local Golden_TestRecursiveBindings = (function()
  local whereRec = (function()
    local yes0
    local no1
    yes0 = function(v2)
      return (function()
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
      end)()
    end
    no1 = function(v3)
      return (function()
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
      end)()
    end
    return no1(false)
  end)()
  local letRecMixed = (function()
    local z4 = 1
    local b5
    local a6
    b5 = function() return a6(z4) end
    a6 = function() return b5(z4) end
    local f7 = function() return function(k10) return a6(k10) end end
    local y8 = f7(z4)(z4)
    local x9 = f7(y8)(y8)
    return f7(x9)(f7(y8)(0))
  end)()
  local letRec = (function()
    local yes11
    local no12
    yes11 = function(v13)
      return (function()
        if true == v13 then
          return no12(false)
        else
          return (function()
            if false == v13 then
              return no12(true)
            else
              return error("No patterns matched")
            end
          end)()
        end
      end)()
    end
    no12 = function(v14)
      return (function()
        if true == v14 then
          return yes11(false)
        else
          return (function()
            if false == v14 then
              return yes11(true)
            else
              return error("No patterns matched")
            end
          end)()
        end
      end)()
    end
    return no12(false)
  end)()
  return { letRec = letRec, whereRec = whereRec, letRecMixed = letRecMixed }
end)()
