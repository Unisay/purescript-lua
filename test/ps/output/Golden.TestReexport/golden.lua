local Golden_Reexport_Exports = (function()
  local binding1 = 1
  return { binding1 = binding1 }
end)()
local Golden_Reexport_ReExports = (function()
  local binding2 = 2
  return { binding2 = binding2 }
end)()
local Golden_TestReexport = (function()
  local binding3 = {
    Golden_Reexport_Exports.binding1,
    Golden_Reexport_ReExports.binding2
  }
  return { binding3 = binding3 }
end)()
