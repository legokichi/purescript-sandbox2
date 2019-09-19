{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "affjax"
    , "arrays"
    , "avar"
    , "canvas"
    , "colors"
    , "console"
    , "const"
    , "control"
    , "coroutines"
    , "css"
    , "effect"
    , "either"
    , "errors"
    , "free"
    , "freet"
    , "functions"
    , "functors"
    , "gen"
    , "globals"
    , "halogen"
    , "halogen-css"
    , "identity"
    , "identy"
    , "lists"
    , "math"
    , "maybe"
    , "parallel"
    , "profunctor"
    , "profunctor-lenses"
    , "promises"
    , "psci-support"
    , "record"
    , "record-extra"
    , "record-format"
    , "refs"
    , "run"
    , "run-streaming"
    , "signal"
    , "sodium"
    , "st"
    , "strings"
    , "struct"
    , "subcategory"
    , "tailrec"
    , "transformers"
    , "tuples"
    , "typelevel-prelude"
    , "uuid"
    , "web-events"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
