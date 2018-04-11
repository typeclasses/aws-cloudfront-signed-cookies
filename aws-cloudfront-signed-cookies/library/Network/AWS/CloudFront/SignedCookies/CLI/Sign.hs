{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- | Command-line interface for generating AWS CloudFront signed cookies.

module Network.AWS.CloudFront.SignedCookies.CLI.Sign
  ( main, mainOpts, Opts (..), optsParser, mainParserInfo
  ) where

import Network.AWS.CloudFront.SignedCookies
import Network.AWS.CloudFront.SignedCookies.CLI.Internal

-- base
import Data.Coerce (Coercible, coerce)
import Data.Foldable (for_)
import Data.Semigroup ((<>))

-- optparse-applicative
import qualified Options.Applicative as Opt

-- text
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- time
import Data.Time.Clock (nominalDay)

-- | Entry point for the AWS CloudFront cookie-signing command-line interface.

main :: IO ()
main = do

  opts <- Opt.execParser $ Opt.info (Opt.helper <*> optsParser) infoMod

  mainOpts opts

mainOpts :: Opts -> IO ()
mainOpts Opts{..} = do

  -- Construct an IAM policy
  policy <- simplePolicy opt_resource opt_lifespan

  -- Parse the .pem file to get the private key
  key <- readPrivateKeyPemFile opt_pemFilePath

  -- Construct signed cookies
  cookies <- createSignedCookies opt_keyPairId key policy

  -- Print the cookies to stdout
  (Text.putStr . renderCookiesText) cookies

-- | Parse result for the command-line arguments for signed cookie generation.

data Opts =
  Opts
    { opt_pemFilePath :: PemFilePath
        -- ^ Location in the filesystem where a .pem file
        --   containing an RSA secret key can be found
    , opt_keyPairId   :: KeyPairId
        -- ^ CloudFront key pair ID for the key pair that
        --   you are using to generate signature
    , opt_resource    :: Resource
        -- ^ URL that the policy will grant access to,
        --   optionally containing asterisks for wildcards
    , opt_lifespan    :: Lifespan
        -- ^ Amount of time until the policy expires
    }

-- | Parser for all the command-line arguments for signed cookie generation.
--
-- See "Options.Applicative", 'Opt.info', and 'Opt.execParser'
-- to learn how to use a 'Opt.Parser'.

optsParser :: Opt.Parser Opts
optsParser =
  Opts
    <$> text "pem-file"    "Location in the filesystem where a .pem file \
                           \containing an RSA secret key can be found"

    <*> text "key-pair-id" "CloudFront key pair ID for the key pair that \
                           \you are using to generate signature"

    <*> text "resource"    "URL that the policy will grant access to, \
                           \optionally containing asterisks for wildcards"

    <*> days "days"        "Integer number of days until the policy expires"

mainParserInfo :: Opt.ParserInfo (IO ())
mainParserInfo =
  mainOpts <$> Opt.info optsParser infoMod

infoMod :: Opt.InfoMod a
infoMod =
  Opt.header "Generate signed cookies for AWS CloudFront"
