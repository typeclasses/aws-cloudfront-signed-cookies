-- |
-- Example usage:
--
-- > {-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- >
-- > import Network.AWS.CloudFront.SignedCookies
-- >
-- > import qualified Data.Text.IO
-- >
-- > main :: IO ()
-- > main = do
-- >
-- >   -- Construct an IAM policy that expires three days from now
-- >   policy :: Policy <- simplePolicy
-- >     (Resource "https://example.com/secrets/*")
-- >     (Lifespan (3 * nominalDay))
-- >
-- >   -- Parse the .pem file to get the private key
-- >   key :: PrivateKey <- readPrivateKeyPemFile
-- >     (PemFilePath "./pk-APKAIATXN3RCIOVT5WRQ.pem")
-- >
-- >   -- Construct signed cookies
-- >   cookies :: CookiesText <- createSignedCookies
-- >     (KeyPairId "APKAIATXN3RCIOVT5WRQ") key policy
-- >
-- >   Data.Text.IO.putStrLn (renderCookiesText cookies)
--
-- Output:
--
-- > Cookie: CloudFront-Policy=eyJTdGF0ZW1lbnQiOlt7IlJlc29...
-- > Cookie: CloudFront-Signature=wMN6V3Okxk7sdSPZeebMh-wo...
-- > Cookie: CloudFront-Key-Pair-Id=APKAIATXN3RCIOVT5WRQ
--
-- You can see a very similar example in action in the
-- "Network.AWS.CloudFront.SignedCookies.CLI" module.

module Network.AWS.CloudFront.SignedCookies

  (
  -- * Creating signed cookies
    createSignedCookies

  -- ** Defining a CloudFront policy
  , simplePolicy
  , Policy (..)
  , Resource (..)
  , Lifespan (..)
  , StartTime (..)
  , EndTime (..)
  , IpAddress (..)

  -- ** Getting your private key
  , readPrivateKeyPemFile
  , PemFilePath (..)
  , KeyPairId (..)
  , PrivateKey

  -- * Policy JSON
  , policyJSON
  , jsonTextPolicy
  , jsonValPolicy

  -- * Reading cookies
  , cookiePolicy

  -- * Miscellaneous

  -- ** Cookies
  , CookiesText
  , renderCookiesText
  , PolicyCookie (..)
  , SignatureCookie (..)

  -- ** Time
  , NominalDiffTime
  , POSIXTime
  , nominalDay
  , getPOSIXTime

  -- ** Text
  , Text

  ) where

import Network.AWS.CloudFront.SignedCookies.Crypto
import Network.AWS.CloudFront.SignedCookies.Encoding
import Network.AWS.CloudFront.SignedCookies.Policy
import Network.AWS.CloudFront.SignedCookies.Types

-- aeson
import qualified Data.Aeson as A

-- base
import Control.Monad ((>=>))
import Data.Coerce (coerce)

-- bytestring
import qualified Data.ByteString.Lazy as LBS

-- text
import qualified Data.Text as Text

-- time
import Data.Time.Clock (nominalDay)
import Data.Time.Clock.POSIX (getPOSIXTime)

createSignedCookies
  :: KeyPairId
    -- ^ A CloudFront key pair ID, which must be associated with a
    --   trusted signer in the CloudFront distribution that you
    --   specify in the 'policyResource'.
  -> PrivateKey
    -- ^ The private key associated with the 'KeyPairId'. See
    --   'readPrivateKeyPemFile' for how to read this key from a
    --   @.pem@ file you downloaded from AWS.
  -> Policy
    -- ^ The policy specifies what resource is being granted, for what
    --   time period, and to what IP addresses. Construct a policy
    --   using the 'Policy' constructor or with the 'simplePolicy'
    --   function.
  -> IO CookiesText

createSignedCookies kpid key policy = do

  let
    policyBS :: ByteString = policyJSON policy

  sigBS <- sign key policyBS

  pure
    [ ( "CloudFront-Policy"      , base64Encode policyBS        )
    , ( "CloudFront-Signature"   , base64Encode sigBS           )
    , ( "CloudFront-Key-Pair-Id" , coerce @KeyPairId @Text kpid )
    ]

-- | Format a list of cookies as HTTP request headers.
renderCookiesText :: CookiesText -> Text
renderCookiesText =
  Text.unlines . map (\(k, v) -> "Cookie: " <> k <> "=" <> v)

-- | Parse the text value of a @CloudFront-Policy@ cookie into a 'Policy'
-- value, producing 'Left' with an error message if parsing fails.
cookiePolicy :: PolicyCookie -> Either String Policy
cookiePolicy =
  (base64Decode . coerce @PolicyCookie @Text) >=>
  (A.eitherDecode' . LBS.fromStrict) >=> jsonValPolicy
