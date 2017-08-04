module PreciseRational
  ( PreciseRational
  , fromInt
  , fromStrings
  , fromBI
  , fromInts
  ) where

import Prelude

import Data.BigInt as BI

import Data.Ratio (Ratio, (%))
import Data.Maybe (Maybe (..))

-- | Type alias for a `Ratio` of `BigInt`s
type PreciseRational = Ratio BI.BigInt

-- | Create a `PreciseRational` from an `Int` with denominator one
fromInt :: Int -> PreciseRational
fromInt i = (BI.fromInt i) % one

-- | Create a `PreciseRational` from two `Int`s as numerator and denominator
fromInts :: Int -> Int -> PreciseRational
fromInts n d = (BI.fromInt n % BI.fromInt d)

-- | Create a `PreciseRational` from an `BigInt` with denominator one
fromBI :: BI.BigInt -> PreciseRational
fromBI bi = bi % one

-- | Try to create a `PreciseRational` from two `String`s as numerator and
-- | denominator
fromStrings :: String -> String -> Maybe PreciseRational
fromStrings ns ds = do
    n <- BI.fromString ns
    d <- BI.fromString ds
    pure (n % d)
