module PreciseRational where


import Prelude
import Data.BigInt as BI

import Data.BigInt (BigInt (..))
import Data.Ratio (Ratio(..))


newtype PreciseRational = PreciseRational (Ratio BigInt)

runPreciseRational :: PreciseRational -> Ratio BigInt
runPreciseRational (PreciseRational ratio) = ratio

fromInt :: Int -> Int -> PreciseRational
fromInt n d = PreciseRational $ Ratio (BI.fromInt n) (BI.fromInt d)
