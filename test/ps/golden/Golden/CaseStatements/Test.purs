module Golden.CaseStatements.Test where

import Golden.Values.Test (f)

a ∷ Int
a = 1

b ∷ Char
b = 'b'

c ∷ Int
c =
  case a, b of
    1, 'b' | f 2, f 1 -> 42
    2, _ | f 0 -> 10
    y@(z@3), _ | f z -> y
    y@(z@4), _ | f y -> z
    _, _ -> 0

{-

let v = \v1 -> 0
in
  case a, b of
    1, 'b' ->
      case f 2 of
        true ->
          case f 1 of
            true -> 42
            _ -> v true
        _ -> v true
    _, _ -> v true

-}

data M a = J a | N

d :: M Int -> M String -> Char -> Int
d m n x =
  case x of
    'x' | J y <- m, N <- n -> y
    'y' -> 0
    _ -> 1

{-

\m -> \n -> \x ->
  let v = \v1 ->
        case x of
          'y' -> 0
          _ -> 1
  'x' ->


-}

multipleGuards ∷ Int
multipleGuards
  | false = 0
  | true = 1

{-

 Case
    [ ]
    [ CaseAlternative
        { caseAlternativeBinders = []
        , caseAlternativeResult =
            Left
              [ ( Literal (BooleanLiteral False)
                , Literal (NumericLiteral (Left 0))
                )
              , ( Literal (BooleanLiteral True)
                , Literal (NumericLiteral (Left 1))
                )
              ]
        }
    ]

-}
