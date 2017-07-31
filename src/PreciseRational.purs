module PreciseRational
  ( PreciseRational
  , fromInt
  , fromBI
  ) where

import Prelude

import Data.BigInt as BI

import Data.Ratio (Ratio, (%))

-- | Type alias for a `Ratio` of `BigInt`s
type PreciseRational = Ratio BI.BigInt

-- | Create a `PreciseRational` from an `Int` with denominator one
fromInt :: Int -> PreciseRational
fromInt i = (BI.fromInt i) % one

-- | Create a `PreciseRational` from an `BigInt` with denominator one
fromBI :: BI.BigInt -> PreciseRational
fromBI bi = bi % one
