local Golden_TestUnbinding_I_f = function(unused1)
  return function(unused0) return 3 end
end
return {
  a = 1,
  b = 2,
  f = Golden_TestUnbinding_I_f,
  c = Golden_TestUnbinding_I_f(1)(Golden_TestUnbinding_I_f(2)(1))
}
