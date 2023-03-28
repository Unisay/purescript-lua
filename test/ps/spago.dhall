{ name = "test-project"
, dependencies = [ ] : List Text
, packages = ./packages.dhall
, sources =
    [ "src/**/*.purs"
    , "golden/**/*.purs"
    ]
}
