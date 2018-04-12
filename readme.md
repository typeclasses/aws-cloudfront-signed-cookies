# aws-cloudfront-signed-cookies

Generate signed cookies for AWS CloudFront

One way to [serve private content through AWS CloudFront](https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html) is to use [signed cookies](https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-signed-cookies.html). This package helps you generate signed cookies [using a custom IAM policy](https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-setting-signed-cookie-custom-policy.html) which may include a range of time for which the cookie is valid and an IP address restriction.

## The library

### Creating signed cookies

Example usage:

```haskell
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Network.AWS.CloudFront.SignedCookies

import qualified Data.Text.IO

main :: IO ()
main = do

  -- Construct an IAM policy that expires three days from now
  policy :: Policy <- simplePolicy
    (Resource "https://example.com/secrets/*")
    (Lifespan (3 * nominalDay))

  -- Parse the .pem file to get the private key
  key :: PrivateKey <- readPrivateKeyPemFile
    (PemFilePath "./pk-APKAIATXN3RCIOVT5WRQ.pem")

  -- Construct signed cookies
  cookies :: CookiesText <- createSignedCookies
    (KeyPairId "APKAIATXN3RCIOVT5WRQ") key policy

  Data.Text.IO.putStrLn (renderCookiesText cookies)
```

The output should look something like this:

```haskell
Cookie: CloudFront-Policy=eyJTdGF0ZW1lbnQiOlt7IlJlc291cmNlIjoiaHR0cHM6Ly9leGFtcGxlLmNvbS9zZWNyZXRzLyoiLCJDb25kaXRpb24iOnsiRGF0ZUxlc3NUaGFuIjp7IkFXUzpFcG9jaFRpbWUiOjE1MjM2NjQxMjV9fX1dfQ__
Cookie: CloudFront-Signature=R17isgiJD36sb4kh4pNxqq5eFCx5Q~vC~QbQKMhi9BqyhLboLQfb3pCdPp-WSVYOn4wlOobqhgcwoX2vjCd3y8eiUtelX1~ddaDlPBkkdZA~5Imd2z-W1o3r0coKB1SE2SPFT7M1XgNvwOPanQoft1VLchfVPGaitFYMum4KS~GXffqQZzaVybKJ64KfFLLBPSobg8MmBhHvpO9DBiwKijmGhDip~6L3W7OcqGT8HdqAmWxjnTHInXpx7bbVotla6a~J6WB6xmtJrmQzMLdTUxAY6IbJ2PzMtTXKgdNzbByGHvImg9k3Q3fNBTim8l7Tds1zpwX9GsuPcFkPe9HZ1Q__
Cookie: CloudFront-Key-Pair-Id=APKAIATXN3RCIOVT5WRQ
```

You can see a very similar example in action in the `Network.AWS.CloudFront.SignedCookies.CLI.Sign` module which defines the command-line interface for creating signed cookies.

### Decoding policy cookies

After you generate a policy cookie and send it to a client, you may later want to parse it back into a `Policy` value -- for example, to determine whether you need to send a new set of cookies to replace an expired policy.

Example in GHCi:

```
λ> cookiePolicy (PolicyCookie "eyJTdGF0ZW1lbnQiOlt7IlJlc291cmNlIjoiaHR0cHM6Ly9leGFtcGxlLmNvbS9zZWNyZXRzLyoiLCJDb25kaXRpb24iOnsiRGF0ZUxlc3NUaGFuIjp7IkFXUzpFcG9jaFRpbWUiOjE1MjM2NjQxMjV9fX1dfQ__")
Right (Policy {policyResource = Resource "https://example.com/secrets/*", policyStart = StartImmediately, policyEnd = EndTime 1523664125s, policyIpAddress = AnyIp})
```

## The executable

You can also generate cookies using the command-line interface. It provides two commands:

* `sign` - Given a private key and policy options, produces signed cookies and prints them as HTTP request headers.
* `decode` - Decodes a `CloudFront-Policy` cookie and prints it in human-readable JSON format.

### `sign`

```
$ aws-cloudfront-signed-cookies sign --help
Generate signed cookies for AWS CloudFront

Usage: aws-cloudfront-signed-cookies sign --pem-file ARG --key-pair-id ARG
                                          --resource ARG --days ARG

Available options:
  --pem-file ARG           Location in the filesystem where a .pem file
                           containing an RSA secret key can be found
  --key-pair-id ARG        CloudFront key pair ID for the key pair that you are
                           using to generate signature
  --resource ARG           URL that the policy will grant access to, optionally
                           containing asterisks for wildcards
  --days ARG               Integer number of days until the policy expires
  -h,--help                Show this help text
```

Example usage:

```
$ aws-cloudfront-signed-cookies sign           \
    --pem-file pk-APKAIATXN3RCIOVT5WRQ.pem     \
    --key-pair-id APKAIATXN3RCIOVT5WRQ         \
    --resource "https://example.com/secrets/*" \
    --days 2
```

Output:

```
Cookie: CloudFront-Policy=eyJTdGF0ZW1lbnQiOlt7IlJlc291cmNlIjoiaHR0cHM6Ly9leGFtcGxlLmNvbS9zZWNyZXRzLyoiLCJDb25kaXRpb24iOnsiRGF0ZUxlc3NUaGFuIjp7IkFXUzpFcG9jaFRpbWUiOjE1MjM2NjQxMjV9fX1dfQ__
Cookie: CloudFront-Signature=R17isgiJD36sb4kh4pNxqq5eFCx5Q~vC~QbQKMhi9BqyhLboLQfb3pCdPp-WSVYOn4wlOobqhgcwoX2vjCd3y8eiUtelX1~ddaDlPBkkdZA~5Imd2z-W1o3r0coKB1SE2SPFT7M1XgNvwOPanQoft1VLchfVPGaitFYMum4KS~GXffqQZzaVybKJ64KfFLLBPSobg8MmBhHvpO9DBiwKijmGhDip~6L3W7OcqGT8HdqAmWxjnTHInXpx7bbVotla6a~J6WB6xmtJrmQzMLdTUxAY6IbJ2PzMtTXKgdNzbByGHvImg9k3Q3fNBTim8l7Tds1zpwX9GsuPcFkPe9HZ1Q__
Cookie: CloudFront-Key-Pair-Id=APKAIATXN3RCIOVT5WRQ
```

### `decode`

```
$ aws-cloudfront-signed-cookies decode --help
Decode signed AWS CloudFront policy cookies

Usage: aws-cloudfront-signed-cookies decode --policy-cookie ARG

Available options:
  --policy-cookie ARG      The value of a CloudFront-Policy cookie
  -h,--help                Show this help text

```

Example usage:

```
$ aws-cloudfront-signed-cookies decode --policy-cookie eyJTdGF0ZW1lbnQiOlt7IlJlc291cmNlIjoiaHR0cHM6Ly9leGFtcGxlLmNvbS9zZWNyZXRzLyoiLCJDb25kaXRpb24iOnsiRGF0ZUxlc3NUaGFuIjp7IkFXUzpFcG9jaFRpbWUiOjE1MjM2NjQxMjV9fX1dfQ__
{
    "Statement": [
        {
            "Resource": "https://example.com/secrets/*",
            "Condition": {
                "DateLessThan": {
                    "AWS:EpochTime": 1523664125
                }
            }
        }
    ]
}
```
