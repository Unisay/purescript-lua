local Golden_Newtype_Test_I_NT = function(x) return x end
return {
  NT = Golden_Newtype_Test_I_NT,
  f = function(v) return v.foo end,
  g = Golden_Newtype_Test_I_NT
}
