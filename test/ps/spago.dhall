{ name = "test-project"
, dependencies = [ "console", "effect", "prelude" ]
, packages = ./packages.dhall
, sources = [ "golden/**/*.purs" ]
}
