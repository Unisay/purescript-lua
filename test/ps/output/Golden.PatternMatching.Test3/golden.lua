local M = {}
M.Golden_PatternMatching_Test3_greaterThan = function(a1, a2)
  if "Data.Ordering∷Ordering.GT" == ((function()
    local unsafeCoerceImpl = function(lt)
      return function(eq)
        return function(gt)
          return function(x)
            return function(y)
              if x < y then
                return lt
              elseif x == y then
                return eq
              else
                return gt
              end
            end
          end
        end
      end
    end
    return { ordIntImpl = unsafeCoerceImpl }
  end)()).ordIntImpl({ ["$ctor"] = "Data.Ordering∷Ordering.LT" })({
    ["$ctor"] = "Data.Ordering∷Ordering.EQ"
  })({ ["$ctor"] = "Data.Ordering∷Ordering.GT" })(a1)(a2)["$ctor"] then
    return true
  else
    return false
  end
end
return {
  test1 = function(v)
    local a = v.a
    if M.Golden_PatternMatching_Test3_greaterThan(a, 0) then
      return a
    else
      local b = v.b
      if M.Golden_PatternMatching_Test3_greaterThan(b, 0) then
        return b
      else
        return 0
      end
    end
  end,
  test2 = function(v)
    local a = v.a
    if M.Golden_PatternMatching_Test3_greaterThan(a, 0) then
      return a
    else
      local b = v.b
      if M.Golden_PatternMatching_Test3_greaterThan(b, 0) then
        return b
      else
        return 0
      end
    end
  end,
  test3 = function(v)
    local a = v.a
    if M.Golden_PatternMatching_Test3_greaterThan(a, 0) then
      return a
    else
      local b = v.b
      if M.Golden_PatternMatching_Test3_greaterThan(b, 0) then
        return b
      else
        return 0
      end
    end
  end,
  test4 = function(v)
    local a = v.a
    if M.Golden_PatternMatching_Test3_greaterThan(a, 0) then
      return a
    else
      local b = v.b
      if M.Golden_PatternMatching_Test3_greaterThan(b, 0) then
        return b
      else
        return 0
      end
    end
  end,
  test5 = function(v)
    local b = v.b
    local a = v.a
    if M.Golden_PatternMatching_Test3_greaterThan(a, 0) then
      return a
    else
      if M.Golden_PatternMatching_Test3_greaterThan(b, 0) then
        return b
      else
        return 0
      end
    end
  end
}
