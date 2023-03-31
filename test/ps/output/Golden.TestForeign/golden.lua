local Golden_TestForeign_I_foreign = (function()
  local fooBar = 42
  return { foo = fooBar }

end)()
local Golden_TestForeign_I_foo = Golden_TestForeign_I_foreign.foo
