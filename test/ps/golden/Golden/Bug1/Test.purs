module Golden.Bug1.Test where

test :: Int
test =
  let go r = { elem: r.elem }
  in _.elem (go { elem: 1 })
