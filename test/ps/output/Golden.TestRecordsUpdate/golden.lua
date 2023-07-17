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
  test1 = {
    x = 2,
    y = Golden_TestRecordsUpdate_I_r.y,
    z = Golden_TestRecordsUpdate_I_r.z
  },
  test2 = function(v) return { x = v.x, y = false, z = v.z } end,
  test3 = function(v)
    return { x = v.x, y = v.y, z = { z = v.z.z, p = "b" } }
  end,
  test4 = function(v) return _S___object_update(v, { x = 1 }) end
}
