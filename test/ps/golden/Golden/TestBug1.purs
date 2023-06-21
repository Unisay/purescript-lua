module Golden.TestBug1 where

test :: Int
test =
  let go r = { elem: r.elem }
  in _.elem (go { elem: 1 })
