local function _S___object_update(o, patches)
  local o_copy = {}
  for k, v in pairs(o) do
    local patch_v = patches
    if patch_v ~= nil then
      o_copy[k] = patch_v
    else
      o_copy[k] = v
    end
  end
  return o_copy
end
local Golden_TestRecordsUpdate_I_r = {
  x = 1,
  y = true,
  z = { z = "foo", p = "a" }
}
return {
  r = Golden_TestRecordsUpdate_I_r,
  test1 = _S___object_update(Golden_TestRecordsUpdate_I_r, { x = 2 }),
  test2 = function(v) return _S___object_update(v, { y = false }) end,
  test3 = function(v)
    return _S___object_update(v, { z = _S___object_update(v.z, { p = "b" }) })
  end,
  test4 = function(v) return _S___object_update(v, { x = 1 }) end
}
