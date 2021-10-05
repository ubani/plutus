let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210919/packages.dhall sha256:03516fdd4f6d1bd6c9eb5e63cf3af3037bc079459117ab93eb85b6eb46e258a7

let overrides = {=}

let additions =
      { servant-support =
          { dependencies =
            [ "console"
            , "prelude"
            , "either"
            , "foldable-traversable"
            , "effect"
            , "aff"
            , "affjax"
            , "exceptions"
            , "web-xhr"
            , "foreign-generic"
            ]
          , repo = "https://github.com/shmish111/purescript-servant-support"
          , version = "1805f896560751c48a04d3e29f9c109df850d8d3"
          }
      , concurrent-queues =
          { dependencies = [ "aff", "avar" ]
          , repo = "https://github.com/purescript-contrib/purescript-concurrent-queues.git"
          , version = "v1.1.0"
          }
        --   https://github.com/jmackie/purescript-datetime-iso/pull/11
      , datetime-iso =
          { dependencies = [ "newtype", "datetime", "parsing" ]
          , repo = "https://github.com/shmish111/purescript-datetime-iso"
          , version = "3a7cbe9fe22509393ddb6bd271f77c095326f6b3"
          }
      , foreign-generic =
            upstream.foreign-generic
          ⫽ { repo = "https://github.com/shmish111/purescript-foreign-generic"
            , version = "bd412a186bae788967cfc92fd3c5e1415355ff8c"
            }
      , markdown =
          { dependencies =
            [ "const", "datetime", "functors", "lists", "ordered-collections", "parsing", "partial", "precise", "prelude", "strings", "unicode", "validation" ]
          , repo = "https://github.com/input-output-hk/purescript-markdown"
          , version = "b51ee0e4aa04c9e6a5a70f2552a400c3f9cad439"
          }
      , matryoshka =
          { dependencies =
            [ "prelude", "fixed-points", "free", "transformers", "profunctor" ]
          , repo = "https://github.com/slamdata/purescript-matryoshka.git"
          , version = "v0.4.0"
          }
      , filterable =
          { dependencies =
            [ "arrays" , "either" , "foldable-traversable" , "identity" , "lists" , "ordered-collections" ]
          , repo = "https://github.com/LiamGoodacre/purescript-filterable"
          , version = "v3.0.1"
          }
      , numerics =
          { dependencies =
            [ "prelude", "integers", "rationals", "uint", "bigints" ]
          , repo = "https://github.com/Proclivis/purescript-numerics"
          , version = "v0.1.2"
          }
      , precise =
          { dependencies =
            [ "arrays", "console", "effect", "exceptions", "gen", "integers", "lists", "numbers", "prelude", "strings" ]
          , repo = "https://github.com/purescript-contrib/purescript-precise"
          , version = "v5.1.0"
          }
      }

in  upstream ⫽ overrides ⫽ additions
