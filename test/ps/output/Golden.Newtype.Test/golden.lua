PSLUA_Golden_Newtype_Test_NT = function(x) return x end
return {
  NT = PSLUA_Golden_Newtype_Test_NT,
  f = function(v) return v.foo end,
  g = PSLUA_Golden_Newtype_Test_NT
}
