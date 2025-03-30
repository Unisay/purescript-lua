-- lua test/ps/output/Golden.BugListGenericEq.Test/main.lua
local list = dofile("test/ps/output/Golden.BugListGenericEq.Test/golden.lua")
local list1 = list.cons(2)(list.cons(1)(list.Nil))
local list2 = list.cons(2)(list.cons(1)(list.Nil))

local areEqual =
  list.eqList(
  {
    eq = function(a)
      return function(b)
        print("Comparing: ", a, b)
        return a == b
      end
    end
  }
).eq(list1)(list2)
print("Lists are equal: ", areEqual)
