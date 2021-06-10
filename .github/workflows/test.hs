import System.Environment
import System.Process

main =
  do
    ghc <- getEnv "ghc"

    let constraints = case ghc of
            "8.2.2"  -> []
            "8.4.4"  -> []
            "8.6.3"  -> []
            "8.8.1"  -> []
            "8.10.1" -> []

    callProcess "cabal" ("build" : "all" : constraints)
    callProcess "cabal" ("test"  : "all" : constraints)
