local Golden_TestBug1_I_test = (function(v) return v.elem end)((function(r)
  return { elem = r.elem }
end)({ elem = 1 }))
