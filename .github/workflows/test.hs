import System.Environment
import System.Process

main =
  do
    ghc <- getEnv "ghc"

    let constraints = case ghc of
            "8.2.2"  -> [ "aeson" .= "1.4.0.0"
                        , "aeson-pretty" .= "0.8.0"
                        , "asn1-encoding" .= "0.9.5"
                        , "asn1-types" .= "0.3.2"
                        , "base" .= "4.10.*"
                        , "base64-bytestring" .= "1.0.0.2"
                        , "bytestring" .= "0.10.8.1"
                        , "cookie" .= "0.4.3"
                        , "cryptonite" .= "0.25"
                        , "hedgehog" .= "0.6"
                        , "lens" .= "4.15.2"
                        , "lens-aeson" .= "1.0.2"
                        , "neat-interpolation" .= "0.3.2"
                        , "optparse-applicative" .= "0.14.1.0"
                        , "pem" .= "0.2.3"
                        , "text" .= "1.2.3.0"
                        , "time" .= "1.8"
                        , "unordered-containers" .= "0.2.10.0"
                        , "vector" .= "0.12.0.1"
                        ]
            "8.4.4"  -> [ "base" .= "4.11.*"
                        , "base64-bytestring" .= "1.0.0.3"
                        , "cryptonite" .= "0.25"
                        , "hedgehog" .= "0.6.1"
                        , "lens" .= "4.19"
                        , "lens-aeson" .= "1.1"
                        , "vector" .= "0.12.0.3"
                        ]
            "8.6.3"  -> [ "asn1-types" .= "0.3.3"
                        , "base" .= "4.12.*"
                        , "base64-bytestring" .= "1.1.0.0"
                        , "cookie" .= "0.4.4"
                        , "cryptonite" .= "0.27"
                        , "hedgehog" .= "1.0"
                        , "neat-interpolation" .= "0.4"
                        , "optparse-applicative" .= "0.15.0.0"
                        , "time" .= "1.9"
                        , "vector" .= "0.12.1.2"
                        ]
            "8.8.1"  -> [ "aeson" .= "1.5.0.0"
                        , "base" .= "4.13.*"
                        , "base64-bytestring" .= "1.2.0.0"
                        , "cryptonite" .= "0.28"
                        , "hedgehog" .= "1.0.4"
                        , "neat-interpolation" .= "0.5.1.1"
                        , "vector" .= "0.12.2.0"
                        ]
            "8.10.1" -> [ "aeson" .= "1.5.6.0"
                        , "aeson-pretty" .= "0.8.8"
                        , "asn1-encoding" .= "0.9.6"
                        , "asn1-types" .= "0.3.4"
                        , "base" .= "4.14.*"
                        , "base64-bytestring" .= "1.2.0.1"
                        , "bytestring" .= "0.11.1.0"
                        , "cookie" .= "0.4.5"
                        , "cryptonite" .= "0.29"
                        , "hedgehog" .= "1.0.5"
                        , "lens" .= "5.0.1"
                        , "lens-aeson" .= "1.1.1"
                        , "neat-interpolation" .= "0.5.1.2"
                        , "optparse-applicative" .= "0.16.1.0"
                        , "pem" .= "0.2.4"
                        , "text" .= "1.2.4.1"
                        , "time" .= "1.9.3"
                        , "unordered-containers" .= "0.2.14.0"
                        , "vector" .= "0.12.3.0"
                        ]
            "9.0.1"  -> [ "base" .= "4.15.*"
                        ]

    callProcess "cabal" ("build" : "all" : constraints)
    callProcess "cabal" ("test" : "all" : "--enable-tests" : constraints)

x .= y =
    "--constraint=" ++ x ++ "==" ++ y
