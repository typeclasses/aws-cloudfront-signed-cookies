{-# LANGUAGE FlexibleContexts #-}

module Network.AWS.CloudFront.SignedCookies.CLI.Internal
  ( text, days
  ) where

import Network.AWS.CloudFront.SignedCookies
import Network.AWS.CloudFront.SignedCookies.Types

-- base
import Data.Coerce (Coercible, coerce)
import Data.Semigroup ((<>))
import Prelude hiding (mod)

-- optparse-applicative
import qualified Options.Applicative as Opt

-- text
import qualified Data.Text as Text


text :: Coercible Text a => String -> String -> Opt.Parser a
text long help = fmap (coerce . Text.pack) $ Opt.strOption $ mod long help

days :: Coercible NominalDiffTime a => String -> String -> Opt.Parser a
days long help = fmap (coerce . (* nominalDay) . fromInteger) $
  Opt.option Opt.auto $ mod long help

mod :: Opt.HasName f => String -> String -> Opt.Mod f a
mod long help = Opt.long long <> Opt.help help
