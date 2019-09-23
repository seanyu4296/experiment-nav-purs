{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let override =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { "package-name" =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , "package-name" =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ],
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}


let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20190818/packages.dhall sha256:c95c4a8b8033a48a350106b759179f68a695c7ea2208228c522866fd43814dc8

let overrides =
      { react-basic-native =
              upstream.react-basic-native
          //  { repo =
                  "https://github.com/jvliwanag/purescript-react-basic-native.git"
              , version =
                  "d35fefba089af763fed4e985316a99e23e3653b0"
              }
      }

let additions =
      { long =
          { dependencies =
              [ "prelude"
              , "console"
              , "effect"
              , "functions"
              , "foreign"
              , "nullable"
              , "quickcheck"
              , "strings"
              ]
          , repo =
              "https://github.com/zapph/purescript-longs.git"
          , version =
              "83035668ac075ca4a6ea8c35c4a6b2410eb24ffd"
          }
      , precise =
          { dependencies =
              [ "arrays"
              , "globals"
              , "integers"
              , "generics-rep"
              , "strings"
              , "gen"
              , "lists"
              , "exceptions"
              ]
          , repo =
              "https://github.com/purescript-contrib/purescript-precise.git"
          , version =
              "v4.0.0"
          }
      , express =
          { dependencies =
              [ "foreign", "foreign-generic", "node-http", "test-unit", "aff" ]
          , repo =
              "https://github.com/jvliwanag/purescript-express.git"
          , version =
              "5d72cbad627cff4f03fa6a288789f89773864bf0"
          }
      , zrpc =
          { dependencies =
              [ "prelude"
              , "argonaut-core"
              , "console"
              , "datetime"
              , "effect"
              , "foreign"
              , "formatters"
              , "js-date"
              , "long"
              , "milkis"
              , "precise"
              , "record"
              , "strings"
              ]
          , repo =
              "ssh://git@bitbucket.org/zaptag/purescript-zrpc.git"
          , version =
              "e0be377ae93869a536221446a10fd9d2df031726"
          }
      }

in  upstream // overrides // additions
