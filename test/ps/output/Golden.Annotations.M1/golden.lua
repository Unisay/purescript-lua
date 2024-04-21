local M = {}
M.Golden_Annotations_M1_foreign = (function()
  local step = 2
  return {
    dontInlineClosure = function(i)
        return i + step
      end,
    inlineMeLambda = function(i)
        return i + i
      end
  }
end)()
return {
  inlineMe = function(v) if 1 == v then return 2 else return v end end,
  dontInlineClosure = M.Golden_Annotations_M1_foreign.dontInlineClosure,
  inlineMeLambda = M.Golden_Annotations_M1_foreign.inlineMeLambda
}
