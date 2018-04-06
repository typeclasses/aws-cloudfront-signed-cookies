{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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

  ) where

import Network.AWS.CloudFront.SignedCookies.Types

-- aeson
import qualified Data.Aeson as A

-- base
import Data.Semigroup ((<>))

-- bytestring
import qualified Data.ByteString.Lazy as LBS

-- time
import Data.Time.Clock.POSIX (getPOSIXTime)

-- unordered-containers
import qualified Data.HashMap.Strict as Map

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
policyValue policy =
  A.Object $ Map.singleton "Statement" $
    A.Array $ Vec.singleton $
      A.Object $ "Resource" .= resourceValue policy <>
                 "Condition" .= conditionValue policy

resourceValue :: Policy -> A.Value
resourceValue (Policy (Resource x) _ _ _) = A.String x

conditionValue :: Policy -> A.Value
conditionValue (Policy _ start end ip) =
  A.Object $ startCondition <> endCondition <> ipCondition
  where

    startCondition :: A.Object =
      case start of
        StartImmediately -> mempty
        StartTime x -> "DateGreaterThan" .= posixTimeValue x

    endCondition :: A.Object =
      case end of
        EndTime x -> "DateLessThan" .= posixTimeValue x

    ipCondition :: A.Object =
      case ip of
        AnyIp -> mempty
        IpAddress x -> "IpAddress" .= sourceIpValue x

posixTimeValue :: POSIXTime -> A.Value
posixTimeValue =
  A.Object . ("AWS:EpochTime" .=) . A.Number . fromInteger . round

sourceIpValue :: Text -> A.Value
sourceIpValue =
  A.Object . ("AWS:SourceIp" .=) . A.String

(.=) :: Text -> A.Value -> A.Object
(.=) = Map.singleton

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
