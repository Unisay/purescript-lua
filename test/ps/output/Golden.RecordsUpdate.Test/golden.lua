local function PSLUA_object_update(o, patches)
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
local M = {}
M.Golden_RecordsUpdate_Test_r = { x = 1, y = true, z = { z = "foo", p = "a" } }
return {
  r = M.Golden_RecordsUpdate_Test_r,
  test1 = PSLUA_object_update(M.Golden_RecordsUpdate_Test_r, { x = 2 }),
  test2 = function(v) return PSLUA_object_update(v, { y = false }) end,
  test3 = function(v)
    return PSLUA_object_update(v, { z = PSLUA_object_update(v.z, { p = "b" }) })
  end,
  test4 = function(v) return PSLUA_object_update(v, { x = 1 }) end
}
