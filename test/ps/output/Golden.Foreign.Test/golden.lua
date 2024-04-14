return {
  foo = ((function() local fooBar = 42 return { foo = fooBar + 1 } end)()).foo
}
