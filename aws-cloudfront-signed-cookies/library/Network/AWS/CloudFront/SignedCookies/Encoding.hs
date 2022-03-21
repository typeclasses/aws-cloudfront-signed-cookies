module Network.AWS.CloudFront.SignedCookies.Encoding
  ( base64Encode
  , base64Decode
  ) where

import Network.AWS.CloudFront.SignedCookies.Types (ByteString, Text)

-- base64-bytestring
import qualified Data.ByteString.Base64 as Base64

-- text
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

{- |

The base 64 encoding scheme used for AWS CloudFront signed cookies.
We use this to encode both the policy and the signature.

Excerpts from [Setting Signed Cookies Using a Custom Policy](https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-setting-signed-cookie-custom-policy.html):

1. Base64-encode the string using MIME base64 encoding. For more information, see
   [Section 6.8, Base64 Content-Transfer-Encoding](https://tools.ietf.org/html/rfc2045#section-6.8)
   in RFC 2045, MIME (Multipurpose Internet Mail Extensions) Part One:
   Format of Internet Message Bodies.

2. Replace characters that are invalid in a URL query string with
   characters that are valid. The following table lists invalid and
   valid characters.

    > Replace these          With these
    > invalid characters     valid characters
    > ------------------     ----------------
    > +                      - (hyphen)
    > =                      _ (underscore)
    > /                      ~ (tilde)

-}

base64Encode :: ByteString -> Text
base64Encode =
  Text.map charEncode . Text.decodeUtf8 . Base64.encode
  where
    charEncode =
      \case
        '+' -> '-'
        '=' -> '_'
        '/' -> '~'
        x   -> x

{- |

The inverse of 'base64Encode'. Produces a 'Left' value with
an error message if decoding fails.

-}

base64Decode :: Text -> Either String ByteString
base64Decode =
  Base64.decode . Text.encodeUtf8 . Text.map charDecode
  where
    charDecode =
      \case
        '-' -> '+'
        '_' -> '='
        '~' -> '/'
        x   -> x
