return {
  bar = ((function()
    local fooBar = 42

    return {
      foo = fooBar,
      bar = function() return fooBar end,
      baz = function() return true end
    }
  end)()).bar
}
