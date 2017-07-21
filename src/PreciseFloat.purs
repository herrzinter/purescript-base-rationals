module PreciseFloat where


import Prelude
import Data.String as String
import Data.Array as Array
import Data.BigInt as BI

import Data.BigInt (BigInt(..), fromString, pow, toString)
import Data.Ratio (Ratio(..), denominator, numerator, gcd)
import Data.List (List(..), elemIndex, drop, dropWhile, length, reverse, snoc, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either (..))
import Control.Error.Util (note)


data PreciseFloat = PreciseFloat
  { finit         :: BigInt
  , infinit       :: BigInt
  , infinitLength :: Int
  , shift         :: BigInt
  }

instance showPreciseFloat :: Show PreciseFloat where
    show (PreciseFloat pfr) = "{f:" <> toString pfr.finit
        <> "i:" <> toString pfr.infinit
        <> "s:" <> toString pfr.shift
        <> "il:" <> show pfr.infinitLength

derive instance eqPreciseFloat :: Eq PreciseFloat


fromRatio :: Ratio BigInt -> PreciseFloat
fromRatio ratio = loop zero (num0 `shiftLeft` one) Nil zero
  where
    -- Seperate whole/propper part from remainder as algorithm fails to work
    -- with improper ratios
    {propper, remainder: (Ratio num0 den)} = propperize ratio

    loop
      :: BigInt       -- Counter representing the shift
      -> BigInt       -- Current numerator
      -> List BigInt  -- Previous numerators to check for recurrence
      -> BigInt       -- Accumulator
      -> PreciseFloat
    loop shift num nums acc
        | num /= zero = case num `elemIndex` nums of
            -- Numerator is not zero and not recurring -> next loop step
            Nothing ->
              let quotient = num / den
                  remainder = num `mod` den
                  -- Append the *quotient* on the left of the acc eg.
                  -- acc = 123, quotient = 0 -> acc' = 1234
                  acc' = acc `shiftLeft` one + quotient
                  -- The remainder is shifted and applied to the loop
                  num' = remainder `shiftLeft` one
              in loop (shift + one) num' (num : nums) acc'
            -- Numerator from *i* steps before recurrs
            Just i ->
              let iBI = BI.fromInt i + one
                  -- This replaces the i digits on the right by zeros eg.
                  -- accumulator = 123456, iBI 2 -> 123400
                  finitPartOfAcc = acc `shiftRight` iBI `shiftLeft` iBI
              in  PreciseFloat
                    { finit : finitPartOfAcc + propper `shiftLeft` shift
                    , infinit: acc - finitPartOfAcc
                    , infinitLength: i + one
                    , shift
                    }
        -- Numerator is zero -> no recurrence, infinit part equals zero
        | otherwise = PreciseFloat
          { finit: acc + propper `shiftLeft` shift
          , infinit: zero
          , shift
          , infinitLength: zero
          }

toRatio :: PreciseFloat -> Ratio BigInt
toRatio pf@(PreciseFloat pfr)
    | not $ isRecurring pf = Ratio pfr.finit (ten `pow` pfr.shift)
    | otherwise            = Ratio num den
      where
        il = BI.fromInt pfr.infinitLength
        num = (pfr.finit + pfr.infinit) `shiftLeft` il - pfr.finit
        den = (ten `pow` il - one) `shiftLeft` pfr.shift

isRecurring :: PreciseFloat -> Boolean
isRecurring (PreciseFloat pfr) = pfr.infinit /= zero

scale :: PreciseFloat -> BigInt -> PreciseFloat
scale pf factor = fromRatio $ Ratio (num * factor) den
  where
    (Ratio num den) = toRatio pf


-- Helpers

ten = BI.fromInt 10 :: BigInt

shiftLeft :: BigInt -> BigInt -> BigInt
shiftLeft value shift  = value * (ten `pow` shift)

shiftRight :: BigInt -> BigInt -> BigInt
shiftRight value shift = value / (ten `pow` shift)

fromCharList :: List Char -> Either String BigInt
fromCharList = note "Could not convert (List Char) to BigInt"
    <<< fromString
    <<< String.fromCharArray
    <<< Array.fromFoldable

fromBigInt :: BigInt -> List Char
fromBigInt = Array.toUnfoldable <<< String.toCharArray <<< toString

propperize :: Ratio BigInt -> {propper :: BigInt, remainder :: Ratio BigInt}
propperize ratio@(Ratio num den) = {propper, remainder}
  where
    -- Calculate propper part of ratio and substract it to get remainder
    propper   = num / den
    remainder = ratio - Ratio propper one
