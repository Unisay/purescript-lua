{ name = "test-project"
, dependencies = [ "console", "effect", "foldable-traversable", "prelude", "exceptions", "maybe" ]
, packages = ./packages.dhall
, sources = [ "golden/**/*.purs" ]
}
