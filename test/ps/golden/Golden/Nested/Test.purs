module Golden.Nested.Test where

isZero :: Int -> Boolean
isZero 0 = true
isZero _ = false

main :: String
main =
  if isZero 1
    then (if isZero 1 then "ok" else "fine")
    else (if isZero 0 then "ha" else "cool")
