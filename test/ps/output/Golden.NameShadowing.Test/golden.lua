local Golden_NameShadowing_Test_I_f = function(v)
  return function(v1)
    if 1 == v then
      return 1
    else
      return (function() if 1 == v1 then return 2 else return 3 end end)()
    end
  end
end
return {
  b = function(x)
    return function(x1)
      return Golden_NameShadowing_Test_I_f(Golden_NameShadowing_Test_I_f(x)(x1))((function( x2 )
        return Golden_NameShadowing_Test_I_f(x2)(1)
      end)(42))
    end
  end,
  c = Golden_NameShadowing_Test_I_f
}
