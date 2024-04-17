local Golden_Nested_Test_I_isZero = function(v)
  if 0 == v then return true else return false end
end
return {
  isZero = Golden_Nested_Test_I_isZero,
  main = (function()
    if Golden_Nested_Test_I_isZero(1) then
      if Golden_Nested_Test_I_isZero(1) then return "ok" else return "fine" end
    else
      if Golden_Nested_Test_I_isZero(0) then return "ha" else return "cool" end
    end
  end)()
}
