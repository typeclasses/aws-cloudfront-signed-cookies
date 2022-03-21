{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications #-}

module Network.AWS.CloudFront.SignedCookies.Policy
  (
  -- * Defining a policy
    Policy (..)
  , simplePolicy

  -- * Components of a policy
  , Resource (..)
  , StartTime (..)
  , EndTime (..)
  , Lifespan (..)
  , IpAddress (..)

  -- * JSON representation
  , policyJSON
  , jsonTextPolicy
  , jsonValPolicy

  ) where

import Network.AWS.CloudFront.SignedCookies.Types

-- aeson
import qualified Data.Aeson as A (Value, eitherDecode', encode, object)
import qualified Data.Aeson.Types as A (Pair)
import Data.Aeson ((.=), toJSON)

-- base
import Control.Monad ((>=>))
import Data.Semigroup ((<>))

-- bytestring
import qualified Data.ByteString.Lazy as LBS

-- lens
import Control.Lens ((&), (^.), (^?))

-- lens-aeson
import Data.Aeson.Lens (AsNumber (..), AsValue (..), key, nth, _Array, _Object)

-- text
import qualified Data.Text.Encoding as Text

-- time
import Data.Time.Clock.POSIX (getPOSIXTime)

-- vector
import qualified Data.Vector as Vec

{- |

Encode a 'Policy' as JSON, with no whitespace, as AWS requires.

Excerpt from [Setting Signed Cookies Using a Custom Policy](https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-setting-signed-cookie-custom-policy.html):

* "Remove all whitespace (including tabs and newline characters) from the policy statement."

-}

policyJSON :: Policy -> ByteString
policyJSON =
  LBS.toStrict . A.encode . policyValue

policyValue :: Policy -> A.Value
policyValue policy = A.object [ "Statement" .= statement ]
  where
    statement = toJSON @[A.Value]
      [ A.object
        [ "Resource" .= resourceValue policy
        , "Condition" .= conditionValue policy
        ]
      ]

resourceValue :: Policy -> A.Value
resourceValue (Policy (Resource x) _ _ _) = toJSON @Text x

conditionValue :: Policy -> A.Value
conditionValue (Policy _ start end ip) =
  A.object $ startCondition <> endCondition <> ipCondition
  where

    startCondition :: [A.Pair] =
      case start of
        StartImmediately -> []
        StartTime x -> ["DateGreaterThan" .= posixTimeValue x]

    endCondition :: [A.Pair] =
      case end of
        EndTime x -> ["DateLessThan" .= posixTimeValue x]

    ipCondition :: [A.Pair] =
      case ip of
        AnyIp -> []
        IpAddress x -> ["IpAddress" .= sourceIpValue x]

posixTimeValue :: POSIXTime -> A.Value
posixTimeValue x = A.object ["AWS:EpochTime" .= toJSON @Integer (round x) ]

sourceIpValue :: Text -> A.Value
sourceIpValue x = A.object ["AWS:SourceIp" .= toJSON @Text x]

jsonTextPolicy :: Text -> Either String Policy
jsonTextPolicy =
  (A.eitherDecode' . LBS.fromStrict . Text.encodeUtf8) >=>
  jsonValPolicy

jsonValPolicy :: A.Value -> Either String Policy
jsonValPolicy val =
  Policy
    <$> jsonResource  val
    <*> jsonStart     val
    <*> jsonEnd       val
    <*> jsonIpAddress val

jsonResource :: A.Value -> Either String Resource
jsonResource val =
  maybe (Left "Missing \"Resource\"") Right $
    fmap Resource
      (val ^? key "Statement"
            . nth 0
            . key "Resource"
            . _String)

jsonStart :: A.Value -> Either String StartTime
jsonStart val =
  Right $
    maybe StartImmediately (StartTime . fromInteger)
      (val ^? key "Statement"
            . nth 0
            . key "Condition"
            . key "DateGreaterThan"
            . key "AWS:EpochTime"
            . _Integer)

jsonEnd :: A.Value -> Either String EndTime
jsonEnd val =
  maybe (Left "Missing \"DateLessThan\"") Right $
    fmap (EndTime . fromInteger)
      (val ^? key "Statement"
            . nth 0
            . key "Condition"
            . key "DateLessThan"
            . key "AWS:EpochTime"
            . _Integer)

jsonIpAddress :: A.Value -> Either String IpAddress
jsonIpAddress val =
  Right $
    maybe AnyIp IpAddress
      (val ^? key "Statement"
            . nth 0
            . key "Condition"
            . key "IpAddress"
            . key "AWS:SourceIp"
            . _String)

{- |

This function provides one convenient way to construct a simple 'Policy'.

For the full set of policy options, use the 'Policy' constructor directly.

-}

simplePolicy
  :: Resource   -- ^ URL that the policy will grant access to,
                --   optionally containing asterisks for wildcards
  -> Lifespan   -- ^ How long from now the credentials expire
  -> IO Policy

simplePolicy res life = do

  now :: POSIXTime <- getPOSIXTime

  let end = case life of Lifespan x -> EndTime (now + x)

  pure
    Policy
      { policyResource  = res
      , policyEnd       = end
      , policyStart     = StartImmediately
      , policyIpAddress = AnyIp
      }
