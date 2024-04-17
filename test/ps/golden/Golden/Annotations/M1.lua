local step = 2
return {
  dontInlineClosure = (function(i)
    return i + step
  end),
  inlineMeLambda = (function(i)
    return i + i
  end)
}
