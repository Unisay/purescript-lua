{ name = "test-project"
, dependencies = [ "console", "effect", "foldable-traversable", "prelude" ]
, packages = ./packages.dhall
, sources = [ "golden/**/*.purs" ]
}
