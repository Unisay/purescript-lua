{ name = "test-project"
, dependencies = [ "effect", "prelude" ]
, packages = ./packages.dhall
, sources = [ "golden/**/*.purs" ]
}
