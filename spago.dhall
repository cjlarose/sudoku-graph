{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "assert"
  , "console"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "maybe"
  , "node-fs"
  , "node-fs-aff"
  , "node-process"
  , "node-readline"
  , "ordered-collections"
  , "parsing"
  , "psci-support"
  , "test-unit"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
