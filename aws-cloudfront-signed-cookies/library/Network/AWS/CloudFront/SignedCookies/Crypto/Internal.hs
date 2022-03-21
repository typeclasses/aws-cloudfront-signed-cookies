{-

This module is based on a part of snaplet-saml. If my hs-certificate PR gets merged, then we probably ought to remove this and use hs-certificate instead.

https://github.com/duairc/snaplet-saml/blob/dd89116f8f0b3d755d2feea73f2c40beb78c409c/src/Data/X509/IO.hs#L87-L102

https://github.com/vincenthz/hs-certificate/pull/98

-}

module Network.AWS.CloudFront.SignedCookies.Crypto.Internal
  ( rsaPrivateKeyFromASN1
  ) where

import Data.ASN1.Types (ASN1 (End, IntVal, Start), ASN1ConstructionType (Sequence))

import qualified Crypto.PubKey.RSA as RSA

rsaPrivateKeyFromASN1 :: [ASN1] -> Either String RSA.PrivateKey

rsaPrivateKeyFromASN1 =
  \case
    ( Start Sequence
        : IntVal version
        : IntVal n
        : IntVal e
        : IntVal d
        : IntVal p
        : IntVal q
        : IntVal dP
        : IntVal dQ
        : IntVal qinv
        : End Sequence
        : [] ) ->

      case version of
        0 -> Right (buildKey Params{..})
        _ -> Left $ "rsaPrivateKeyFromASN1: unexpected version " <>
                    show @Integer version

    _ -> Left "rsaPrivateKeyFromASN1: unexpected format"

-- https://tls.mbed.org/kb/cryptography/asn1-key-structures-in-der-and-pem
data Params =
  Params
    { n    :: Integer -- modulus
    , e    :: Integer -- publicExponent
    , d    :: Integer -- privateExponent
    , p    :: Integer -- prime1
    , q    :: Integer -- prime2
    , dP   :: Integer -- exponent1 = d mod (p-1)
    , dQ   :: Integer -- exponent2 = d mod (q-1)
    , qinv :: Integer -- coefficient = (inverse of q) mod p
    }

buildKey :: Params -> RSA.PrivateKey
buildKey Params{..} =
  let
    size = head [i | i <- [1..], 2 ^ (i * 8) > n]

    pub = RSA.PublicKey
      { public_size = size
      , public_n = n
      , public_e = e
      }

  in
    RSA.PrivateKey
      { RSA.private_pub  = pub
      , RSA.private_d    = d
      , RSA.private_p    = p
      , RSA.private_q    = q
      , RSA.private_dP   = dP
      , RSA.private_dQ   = dQ
      , RSA.private_qinv = qinv
      }
