PSLUA_Golden_NameShadowing_Test_f = function(v)
  return function(v1)
    if 1 == v then return 1 else if 1 == v1 then return 2 else return 3 end end
  end
end
return {
  b = function(x)
    return function(x1)
      return PSLUA_Golden_NameShadowing_Test_f(PSLUA_Golden_NameShadowing_Test_f(x)(x1))(PSLUA_Golden_NameShadowing_Test_f(42)(1))
    end
  end,
  c = PSLUA_Golden_NameShadowing_Test_f
}
