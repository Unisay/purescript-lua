local M = {}
M.Golden_Nested_Test_isZero = function(v)
  if 0 == v then return true else return false end
end
return {
  isZero = M.Golden_Nested_Test_isZero,
  main = (function()
    if M.Golden_Nested_Test_isZero(1) then
      if M.Golden_Nested_Test_isZero(1) then return "ok" else return "fine" end
    else
      if M.Golden_Nested_Test_isZero(0) then return "ha" else return "cool" end
    end
  end)()
}
