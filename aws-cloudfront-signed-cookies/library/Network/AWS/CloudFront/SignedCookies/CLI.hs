{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}

-- | Command-line interface for generating AWS CloudFront signed cookies.

module Network.AWS.CloudFront.SignedCookies.CLI
  ( main, Opts (..), optsParser
  ) where

import Network.AWS.CloudFront.SignedCookies

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

-- | The entry point for the AWS CloudFront cookie-signing
-- command-line interface.

main :: IO ()
main = do

  -- Parse the command-line arguments
  Opts{..} <- Opt.execParser $
    Opt.info (Opt.helper <*> optsParser) $
      Opt.header "Generator of signed cookies for AWS CloudFront"

  -- Construct an IAM policy
  policy <- simplePolicy opt_resource opt_lifespan

  -- Parse the .pem file to get the private key
  key <- readPrivateKeyPemFile opt_pemFilePath

  -- Construct signed cookies
  cookies <- createSignedCookies opt_keyPairId key policy

  -- Print the cookies to stdout
  (Text.putStr . renderCookiesText) cookies

-- | The parse result for the command-line arguments.

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

-- | Parser for all of the command-line arguments.
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

  where
    text :: Coercible Text a => String -> String -> Opt.Parser a
    text long help = fmap (coerce . Text.pack) $ Opt.strOption $ mod long help

    days :: Coercible NominalDiffTime a => String -> String -> Opt.Parser a
    days long help = fmap (coerce . (* nominalDay) . fromInteger) $
      Opt.option Opt.auto $ mod long help

    mod :: Opt.HasName f => String -> String -> Opt.Mod f a
    mod long help = Opt.long long <> Opt.help help
