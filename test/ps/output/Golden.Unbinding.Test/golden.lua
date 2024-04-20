PSLUA_Golden_Unbinding_Test_f = function(unused1)
  return function(unused0) return 3 end
end
return {
  a = 1,
  b = 2,
  f = PSLUA_Golden_Unbinding_Test_f,
  c = PSLUA_Golden_Unbinding_Test_f(1)(PSLUA_Golden_Unbinding_Test_f(2)(1))
}
