local M = {}
M.Golden_Foreign_Test_foreign = (function()
  local fooBar = 42
  return { foo = fooBar + 1, boo = fooBar + 2 }
end)()
return {
  foo = M.Golden_Foreign_Test_foreign.foo,
  baz = { [1] = M.Golden_Foreign_Test_foreign.boo, [2] = 100 }
}
