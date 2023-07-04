return {
  foo = ((function() local fooBar = 42 return { foo = fooBar } end)()).foo
}
