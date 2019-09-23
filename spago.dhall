{ name =
    "experiment-nav"
, dependencies =
    [ "aff-promise"
    , "console"
    , "debug"
    , "effect"
    , "express"
    , "foreign-object"
    , "generics-rep"
    , "js-date"
    , "now"
    , "numbers"
    , "prelude"
    , "psci-support"
    , "react-basic"
    , "react-basic-native"
    , "react-basic-hooks"
    , "zrpc"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
