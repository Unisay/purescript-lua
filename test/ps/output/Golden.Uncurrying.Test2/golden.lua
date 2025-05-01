local M = {}
M.Golden_Uncurrying_Test2_uncurried = function(v, v1)
  if 1 == v then
    if 1 == v1 then return true else return false end
  else
    return false
  end
end
return {
  test = function(v)
    local a = v.a
    if M.Golden_Uncurrying_Test2_uncurried(a, 0) then
      return a
    else
      local b = v.b
      if M.Golden_Uncurrying_Test2_uncurried(b, 0) then
        return b
      else
        return 0
      end
    end
  end
}
