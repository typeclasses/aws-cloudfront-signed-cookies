module Network.AWS.CloudFront.SignedCookies.Types

  (
  -- * Policy
    Policy (..), Resource (..)

  -- * Crypto
  , PemFilePath (..), KeyPairId (..)

  -- * Cookies
  , CookiesText, SetCookie, CookieDomain (..), CookiePath (..)
  , PolicyCookie (..), SignatureCookie (..)

  -- * Time
  , NominalDiffTime, POSIXTime, Lifespan (..), StartTime (..), EndTime (..)

  -- * IP address
  , IpAddress (..)

  -- * Strings
  , Text, ByteString

  -- * Crypto
  , PrivateKey

  ) where

-- bytestring
import Data.ByteString (ByteString)

-- cookie
import Web.Cookie (CookiesText, SetCookie)

-- cryptonite
import Crypto.PubKey.RSA (PrivateKey (..), PublicKey (..))

-- text
import Data.Text (Text)

-- time
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)

{- |

Location in the filesystem where a .pem file containing an
RSA secret key can be found.

The filename downloaded from AWS looks like this:

* @"pk-APKAIATX@@N3RCIOVT5WRQ.pem"@

-}
newtype PemFilePath = PemFilePath Text
  deriving (Eq, Show)

{- |

CloudFront key pair ID for the key pair that you are using to
generate signature.

The key pair ID can be found in the name of key files that you
download, and looks like this:

* @APKAIATXN3@@RCIOVT5WRQ@

-}
newtype KeyPairId = KeyPairId Text
  deriving (Eq, Show)

{- |

Examples:

* @"d123example.cl@@oudfront.net"@
* @"cloudfrontalia@@s.example.com"@

-}
newtype CookieDomain = CookieDomain Text
  deriving (Eq, Show)

-- | Usually @"/"@
newtype CookiePath = CookiePath Text
  deriving (Eq, Show)

-- | The value of a @CloudFront-Policy@ cookie.
newtype PolicyCookie = PolicyCookie Text
  deriving (Eq, Show)

-- | The value of a @CloudFront-Signature@ cookie.
newtype SignatureCookie = SignatureCookie Text
  deriving (Eq, Show)

{- |

URL that a policy will grant access to, optionally containing
asterisks for wildcards.

Examples:

* @"https:\/@@\/d123example.cloudfront.net/index.html"@
* @"https:\/@@\/d123example.cloudfront.net/*.jpeg"@

-}
newtype Resource = Resource Text
  deriving (Eq, Show)

-- | How long from now the credentials expire
newtype Lifespan = Lifespan NominalDiffTime
  deriving (Eq, Show)

-- | The time at which credentials begin to take effect
data StartTime = StartImmediately | StartTime POSIXTime
  deriving (Eq, Show)

-- | The time at which credentials expire
newtype EndTime = EndTime POSIXTime
  deriving (Eq, Show)

-- | The IP address or address range of clients allowed to make requests
data IpAddress = AnyIp | IpAddress Text
  deriving (Eq, Show)

{- |

A policy specifies what resource is being granted, for what time period,
and to what IP addresses.

For AWS's documentation on what going into a CloudFront policy statement, see [Values That You Specify in the Policy Statement for a Custom Policy for Signed Cookies](https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-setting-signed-cookie-custom-policy.html#private-content-custom-policy-statement-cookies-values).

-}
data Policy =
  Policy
    { policyResource  :: Resource
        -- ^ URL that the policy will grant access to,
        --   optionally containing asterisks for wildcards
    , policyStart     :: StartTime
        -- ^ The time at which credentials begin to take effect
    , policyEnd       :: EndTime
        -- ^ The time at which credentials expire
    , policyIpAddress :: IpAddress
        -- ^ The IP address or address range of clients allowed to make requests
    }
    deriving (Eq, Show)
