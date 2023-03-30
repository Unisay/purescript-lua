local foreign = (function() local fooBar = 42 return { foo = fooBar }  end)()
local foo = foreign.foo
