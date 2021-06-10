import System.Environment
import System.Process

main =
  do
    ghc <- getEnv "ghc"

    let constraints = case ghc of
            "8.2.2"  -> [ "aeson" .= "1.5.0.0"
                        , "bytestring" .= "0.10.8.1"
                        , "lens" .= "4.19"
                        , "lens-aeson" .= "1.1"
                        , "pem" .= "0.2.4"
                        , "text" .= "1.2.3.0"
                        , "time" .= "1.9.3"
                        , "unordered-containers" .= "0.2.10.0"
                        ]
            "8.4.4"  -> []
            "8.6.3"  -> []
            "8.8.1"  -> []
            "8.10.1" -> [ "aeson" .= "1.5.6.0"
                        , "aeson-pretty" .= "0.8.8"
                        , "asn1-encoding" .= "0.9.6"
                        , "asn1-types" .= "0.3.4"
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
                        , "unordered-containers" .= "0.2.14.0"
                        , "vector" .= "0.12.3.0"
                        ]

    callProcess "cabal" ("build" : "all" : constraints)
    callProcess "cabal" ("test" : "all" : "--enable-tests" : constraints)

x .= y =
    "--constraint=" ++ x ++ "==" ++ y
