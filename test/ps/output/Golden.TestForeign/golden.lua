local Golden_TestForeign = (function()
  local foreign = (function() local fooBar = 42 return { foo = fooBar }  end)()
  local foo = foreign.foo
  return { foo = foo }
end)()
