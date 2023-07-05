return {
  r = { x = 1, y = true },
  test1 = ({ x = 1, y = true }).x,
  test2 = function(v) return v.x end,
  test3 = function(v) return v.x end,
  test4 = function(v) return v.x end
}
