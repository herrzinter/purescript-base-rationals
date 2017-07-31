module PreciseRational
  ( PreciseRational
  , fromInt
  ) where

import Prelude

import Data.BigInt as BI

import Data.Ratio (Ratio, (%))


type PreciseRational = Ratio BI.BigInt

fromInt :: Int -> PreciseRational
fromInt i = (BI.fromInt i) % one

fromBigInt :: BI.BigInt -> PreciseRational
fromBigInt bi = bi % one
