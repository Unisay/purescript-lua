local Golden_TestForeign_I_foo = ((function()
  local fooBar = 42
  return { foo = fooBar }
end)()).foo
