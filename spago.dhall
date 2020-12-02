{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "maybe"
  , "node-fs"
  , "node-process"
  , "node-readline"
  , "ordered-collections"
  , "parsing"
  , "psci-support"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
