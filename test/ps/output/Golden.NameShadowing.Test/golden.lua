local Golden_NameShadowing_Test_I_f = function(v)
  return function(v1)
    if 1 == v then return 1 else if 1 == v1 then return 2 else return 3 end end
  end
end
return {
  b = function(x)
    return function(x1)
      return Golden_NameShadowing_Test_I_f(Golden_NameShadowing_Test_I_f(x)(x1))(Golden_NameShadowing_Test_I_f(42)(1))
    end
  end,
  c = Golden_NameShadowing_Test_I_f
}
