local Golden_Annotations_M1_I_foreign = (function()
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
  inlineIntoMe = function(i)
    if 1 == (function()
      if 1 == (function() if 1 == i then return 2 else return i end end)() then
        return 2
      else
        if 1 == i then return 2 else return i end
      end
    end)() then
      return 2
    else
      if 1 == (function() if 1 == i then return 2 else return i end end)() then
        return 2
      else
        if 1 == i then return 2 else return i end
      end
    end
  end,
  inlineIntoMe2 = Golden_Annotations_M1_I_foreign.dontInlineClosure(Golden_Annotations_M1_I_foreign.inlineMeLambda(Golden_Annotations_M1_I_foreign.inlineMeLambda(17)))
}
