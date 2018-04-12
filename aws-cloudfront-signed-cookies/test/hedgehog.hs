{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Main (main) where

import Network.AWS.CloudFront.SignedCookies

-- base
import Control.Monad (unless)
import Data.Foldable (for_)
import qualified System.Exit as Exit
import qualified System.IO   as IO

-- hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- neat-interpolation
import NeatInterpolation

main :: IO ()
main = do
  for_ [IO.stdout, IO.stderr] $ \h -> do
    IO.hSetEncoding h IO.utf8
    IO.hSetBuffering h IO.LineBuffering
  success <- Hedgehog.checkParallel $$(Hedgehog.discover)
  unless success Exit.exitFailure

prop_jsonPolicy_1 :: Property
prop_jsonPolicy_1 = withTests 1 $ property $
  let
    x = [text|
        {
          "Statement": [
              {
                 "Resource": "http://d111111abcdef8.cloudfront.net/game_download.zip",
                 "Condition": {
                    "IpAddress": {"AWS:SourceIp": "192.0.2.0/24"},
                    "DateLessThan": {"AWS:EpochTime": 1357034400}
                 }
              }
           ]
        }
      |]

    p = Policy
      (Resource "http://d111111abcdef8.cloudfront.net/game_download.zip")
      StartImmediately
      (EndTime 1357034400)
      (IpAddress "192.0.2.0/24")
  in
    jsonTextPolicyMaybe x === Just p

prop_jsonPolicy_2 :: Property
prop_jsonPolicy_2 = withTests 1 $ property $
  let
    x = [text|
        {
           "Statement": [
              {
                 "Resource": "http://*",
                 "Condition": {
                    "IpAddress": {"AWS:SourceIp": "192.0.2.10/32"},
                    "DateGreaterThan": {"AWS:EpochTime": 1357034400},
                    "DateLessThan": {"AWS:EpochTime": 1357120800}
                 }
              }
           ]
        }
      |]

    p = Policy
      (Resource "http://*")
      (StartTime 1357034400)
      (EndTime 1357120800)
      (IpAddress "192.0.2.10/32")
  in
    jsonTextPolicyMaybe x === Just p

prop_jsonPolicy_3 :: Property
prop_jsonPolicy_3 = withTests 1 $ property $
  let
    x = [text|
        {
           "Statement": [
              {
                 "Resource": "http://*",
                 "Condition": {
                    "IpAddress": {"AWS:SourceIp": "192.0.2.10/32"},
                    "DateGreaterThan":
      |]

  in
    jsonTextPolicyMaybe x === Nothing
