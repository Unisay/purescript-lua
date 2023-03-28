local Golden_TestRecDataDefs = (function()
  local A = function() return { ["$ctor"] = "Golden_TestRecDataDefs.A" } end
  local AB = function(value0)
    return { ["$ctor"] = "Golden_TestRecDataDefs.AB", value0 = value0 }
  end
  local B = function() return { ["$ctor"] = "Golden_TestRecDataDefs.B" } end
  local BA = function(value0)
    return { ["$ctor"] = "Golden_TestRecDataDefs.BA", value0 = value0 }
  end
  local b = B
  local ab = AB(b)
  local ba = BA(ab)
  local a = A
  return { A = A, AB = AB, B = B, BA = BA, a = a, b = b, ab = ab, ba = ba }
end)()
