{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, TypeApplications #-}

-- | Command-line interface for decoding AWS CloudFront policy cookies.

module Network.AWS.CloudFront.SignedCookies.CLI.Decode
  ( main, mainOpts, Opts (..), optsParser, mainParserInfo
  ) where

import Network.AWS.CloudFront.SignedCookies
import Network.AWS.CloudFront.SignedCookies.CLI.Internal
import Network.AWS.CloudFront.SignedCookies.Encoding

-- aeson
import qualified Data.Aeson as Aeson

-- aeson-pretty
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)

-- base
import Data.Coerce (Coercible, coerce)
import Data.Foldable (for_)
import Data.Semigroup ((<>))

-- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- optparse-applicative
import qualified Options.Applicative as Opt

-- text
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import Data.Text.Lazy.Builder as Text

-- time
import Data.Time.Clock (nominalDay)

-- | Entry point for the AWS CloudFront cookie decoding command-line interface.

main :: IO ()
main = do

  opts <- Opt.execParser $ Opt.info (Opt.helper <*> optsParser) infoMod

  mainOpts opts

mainOpts :: Opts -> IO ()
mainOpts Opts{..} = do

  bs :: BS.ByteString <-
    case base64Decode $ coerce @PolicyCookie @Text opt_policyCookie of
      Left e  -> fail e
      Right x -> pure x

  value <-
    case Aeson.eitherDecode' (LBS.fromStrict bs) of
      Left e  -> fail e
      Right x -> pure x

  let
    result :: LText.Text =
      Text.toLazyText $ encodePrettyToTextBuilder @Aeson.Value value

  -- Print the JSON to stdout
  LText.putStr result

-- | Parse result for the command-line arguments for cookie decoding.

data Opts =
  Opts
    { opt_policyCookie :: PolicyCookie
        -- ^ The value of a @CloudFront-Policy@ cookie.
    }

-- | Parser for all of the command-line arguments for cookie decoding.
--
-- See "Options.Applicative", 'Opt.info', and 'Opt.execParser'
-- to learn how to use a 'Opt.Parser'.

optsParser :: Opt.Parser Opts
optsParser =
  Opts
    <$> text "policy-cookie" "The value of a CloudFront-Policy cookie"

mainParserInfo :: Opt.ParserInfo (IO ())
mainParserInfo =
  mainOpts <$> Opt.info optsParser infoMod

infoMod :: Opt.InfoMod a
infoMod =
  Opt.header "Decode signed AWS CloudFront policy cookies"
