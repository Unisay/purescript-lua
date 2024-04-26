local M = {}
M.Data_Semiring_foreign = {
  intAdd = function(x) return function(y) return x + y end end,
  intMul = function(x) return function(y) return x * y end end
}
M.Data_Semiring_semiringInt = {
  add = M.Data_Semiring_foreign.intAdd,
  zero = 0,
  mul = M.Data_Semiring_foreign.intMul,
  one = 1
}
M.Golden_Fibonacci_Test_sub = function(x) return function(y) return x - y end end
M.Golden_Fibonacci_Test_fib = function(v)
  if 0 == v then
    return 0
  else
    if 1 == v then
      return 1
    else
      return M.Data_Semiring_semiringInt.add(M.Golden_Fibonacci_Test_fib(M.Golden_Fibonacci_Test_sub(v)(1)))(M.Golden_Fibonacci_Test_fib(M.Golden_Fibonacci_Test_sub(v)(2)))
    end
  end
end
return (function(s) return function() print(s) end end)((function(n) return tostring(n) end)(M.Golden_Fibonacci_Test_fib(32)))()
