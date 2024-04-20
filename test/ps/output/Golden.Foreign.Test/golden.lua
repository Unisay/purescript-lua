PSLUA_Golden_Foreign_Test_foreign = (function()
  local fooBar = 42
  return { foo = fooBar + 1, boo = fooBar + 2 }
end)()
return {
  foo = PSLUA_Golden_Foreign_Test_foreign.foo,
  baz = { [1] = PSLUA_Golden_Foreign_Test_foreign.boo, [2] = 100 }
}
