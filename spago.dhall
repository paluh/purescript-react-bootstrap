{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "atleast"
  , "bifunctors"
  , "console"
  , "control"
  , "convertable-options"
  , "datetime"
  , "datetime-iso"
  , "debug"
  , "decimals"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "formatters"
  , "functors"
  , "halogen-subscriptions"
  , "identity"
  , "integers"
  , "js-timers"
  , "maybe"
  , "newtype"
  , "nullable"
  , "ordered-collections"
  , "parsing"
  , "polyform"
  , "polyform-batteries-core"
  , "polyform-batteries-urlencoded"
  , "prelude"
  , "profunctor"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "record"
  , "refs"
  , "safe-coerce"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "undefined-is-not-a-problem"
  , "unsafe-coerce"
  , "validation"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
