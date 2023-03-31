{ name = "test-project"
, dependencies = [ "lua-effect", "lua-prelude" ]
, packages = ./packages.dhall
, sources = [ "golden/**/*.purs" ]
}
