{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}

-- | Command-line interface for generating AWS CloudFront signed cookies.
--
-- This program is the aggregation of these subcommands:
--
-- * "Network.AWS.CloudFront.SignedCookies.CLI.Decode"
-- * "Network.AWS.CloudFront.SignedCookies.CLI.Sign"

module Network.AWS.CloudFront.SignedCookies.CLI
  ( main
  ) where

import Network.AWS.CloudFront.SignedCookies.CLI.Internal

import qualified Network.AWS.CloudFront.SignedCookies.CLI.Decode as Decode
import qualified Network.AWS.CloudFront.SignedCookies.CLI.Sign   as Sign

-- base
import Control.Monad (join)
import Data.Semigroup ((<>))

-- optparse-applicative
import qualified Options.Applicative as Opt

-- | Entry point for the AWS CloudFront signed cookie command-line interface.

main :: IO ()
main =
  join $ Opt.execParser mainParserInfo

mainParserInfo :: Opt.ParserInfo (IO ())
mainParserInfo =
  Opt.info (Opt.helper <*> mainParser) $
    Opt.header "AWS CloudFront signed cookie utility"

mainParser :: Opt.Parser (IO ())
mainParser =
  Opt.hsubparser $
    Opt.command "decode" Decode.mainParserInfo <>
    Opt.command "sign"   Sign.mainParserInfo
