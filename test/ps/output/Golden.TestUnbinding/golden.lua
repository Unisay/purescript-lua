local Golden_TestUnbinding_I_f = function(v0)
  return function(v11) return 3 end
end
local Golden_TestUnbinding_I_b = 2
local Golden_TestUnbinding_I_a = 1
local Golden_TestUnbinding_I_c = Golden_TestUnbinding_I_f(Golden_TestUnbinding_I_a)(Golden_TestUnbinding_I_f(Golden_TestUnbinding_I_b)(Golden_TestUnbinding_I_a))
