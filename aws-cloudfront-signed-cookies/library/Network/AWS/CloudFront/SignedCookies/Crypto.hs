{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Network.AWS.CloudFront.SignedCookies.Crypto
  (
  -- * Reading the private key
    readPrivateKeyPemFile

  -- * Generating signatures
  , sign

  -- * Types
  , PrivateKey
  , ByteString

  ) where

import Network.AWS.CloudFront.SignedCookies.Crypto.Internal
import Network.AWS.CloudFront.SignedCookies.Types

-- asn1-encoding
import Data.ASN1.BinaryEncoding (DER (DER))
import Data.ASN1.Encoding (decodeASN1')
import Data.ASN1.Error (ASN1Error)

-- asn1-types
import Data.ASN1.Types (ASN1)

-- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- cryptonite
import Crypto.PubKey.RSA (PrivateKey)
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import Crypto.Hash.Algorithms (SHA1 (SHA1))

-- pem
import qualified Data.PEM as PEM

-- text
import qualified Data.Text as Text

-- | Construct the signature that will go into the
--   @CloudFront-Signature@ cookie.

sign
  :: PrivateKey     -- ^ The RSA private key that you read from the @.pem@ file
  -> ByteString     -- ^ The JSON representation of the 'Policy'
                    --   (see "Network.AWS.CloudFront.SignedCookies.Policy")
  -> IO ByteString

sign key bs =
  RSA.signSafer (Just SHA1) key bs >>= either (fail . show) pure

-- | Read an RSA private key from a @.pem@ file you downloaded from AWS.

readPrivateKeyPemFile
  :: PemFilePath    -- ^ The filesystem path of the @.pem@ file
  -> IO PrivateKey

readPrivateKeyPemFile (PemFilePath path) = do

  lbs <- BS.readFile (Text.unpack path)

  pemSections <- either fail pure (PEM.pemParseBS lbs)

  pemBs <- PEM.pemContent <$> case pemSections of
    [x] -> pure x
    xs ->
      let msg = "Expected exactly 1 PEM section but found " ++
                show @Int (length xs)
      in  fail msg

  asn1s :: [ASN1] <- either (fail . show) pure (decodeASN1' DER pemBs)

  either fail pure (rsaPrivateKeyFromASN1 asn1s)
