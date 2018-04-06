# aws-cloudfront-signed-cookies

Generate signed cookies for AWS CloudFront

One way to [serve private content through AWS CloudFront](https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html) is to use [signed cookies](https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-signed-cookies.html). This package helps you generate signed cookies [using a custom IAM policy](https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-setting-signed-cookie-custom-policy.html) which may include a range of time for which the cookie is valid and an IP address restriction.

## The library

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
Cookie: CloudFront-Policy=eyJTdGF0ZW1lbnQiOlt7IlJlc29...
Cookie: CloudFront-Signature=wMN6V3Okxk7sdSPZeebMh-wo...
Cookie: CloudFront-Key-Pair-Id=APKAIATXN3RCIOVT5WRQ
```

You can see a very similar example in action in the `Network.AWS.CloudFront.SignedCookies.CLI` module which defines the command-line interface.

## The executable

You can also generate cookies using the command-line interface.

```
$ aws-cloudfront-signed-cookies --help
Generator of signed cookies for AWS CloudFront

Usage: aws-cloudfront-signed-cookies --pem-file ARG --key-pair-id ARG
                                     --resource ARG --days ARG

Available options:
  -h,--help                Show this help text
  --pem-file ARG           Location in the filesystem where a .pem file
                           containing an RSA secret key can be found
  --key-pair-id ARG        CloudFront key pair ID for the key pair that you are
                           using to generate signature
  --resource ARG           URL that the policy will grant access to, optionally
                           containing asterisks for wildcards
  --days ARG               Integer number of days until the policy expires
```

Example usage:

```
$ aws-cloudfront-signed-cookies                \
    --pem-file pk-APKAIATXN3RCIOVT5WRQ.pem     \
    --key-pair-id APKAIATXN3RCIOVT5WRQ         \
    --resource "https://example.com/secrets/*" \
    --days 2
```
