{ name = "test-project"
, dependencies =
  [ "console"
  , "effect"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor"
  ]
, packages = ./packages.dhall
, sources = [ "golden/**/*.purs" ]
}
