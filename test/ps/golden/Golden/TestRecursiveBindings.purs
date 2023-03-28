module Golden.TestRecursiveBindings where

letRec :: Boolean
letRec =
  let
    no :: Boolean -> Boolean
    no = case _ of
      true -> yes false
      false -> yes true

    yes :: Boolean -> Boolean
    yes = case _ of
      true -> no false
      false -> no true
  in
    no false

whereRec :: Boolean
whereRec = no false
  where
  no :: Boolean -> Boolean
  no = case _ of
    true -> yes false
    false -> yes true

  yes :: Boolean -> Boolean
  yes = case _ of
    true -> no false
    false -> no true

letRecMixed :: Int
letRecMixed =
  let
    z = 1
    f _ k = a k
    -- ^ non-recursive binding `f` depends on `a` from a mutually recursive group
    x = y `f` y -- x depends on `y` definded below.
    y = z `f` z -- y depends on `z` definded above.

    -- `a` and `b` form a mutually recursive group:
    a _ = b z -- depends on a binding `z` outside of the group
    b _ = a z -- depends on a binding `z` outside of the group

  {-
    The bindings above are sorted by purs in the following order:

    NonRec: z
    Rec: b, a
    NonRec: f
    NonRec: y
    NonRec: x
  -}
  in
    x `f` (y `f` 0)
