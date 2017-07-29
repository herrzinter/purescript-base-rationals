module PreciseFloat
  ( PreciseFloat (..)
  , fromRatio
  , toRatio
  , scale
  , isZero
  , toMixedRatio
  , appendNZerosOnTheRight
  , stripNDigitsOnTheRight
  ) where


import Prelude

import Data.String as String
import Data.BigInt as BI

import Data.BigInt (BigInt(..), pow)
import Data.Ratio (Ratio(..))
import Data.List (List(..), elemIndex, (:))
import Data.Maybe (Maybe(..))


data PreciseFloat = PreciseFloat
  { finit         :: BigInt
  , infinit       :: BigInt
  , infinitLength :: BigInt
  , shift         :: BigInt
  }

instance showPreciseFloat :: Show PreciseFloat where
    show (PreciseFloat pfr) = "{f:" <> BI.toString pfr.finit
        <> " i:" <> BI.toString pfr.infinit
        <> " s:" <> BI.toString pfr.shift
        <> " il:" <> BI.toString pfr.infinitLength
        <> "}"

derive instance eqPreciseFloat :: Eq PreciseFloat


fromRatio :: Ratio BigInt -> PreciseFloat
fromRatio ratio = loop zero (num0 `appendNZerosOnTheRight` one) Nil zero
  where
    -- Seperate whole/propper part from remainder as algorithm fails to work
    -- with improper ratios
    {whole, propper: (Ratio num0 den)} = toMixedRatio ratio

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
                  acc' = acc `appendNZerosOnTheRight` one + quotient
                  -- The remainder is shifted and applied to the loop
                  num' = remainder `appendNZerosOnTheRight` one
              in loop (shift + one) num' (num : nums) acc'
            -- Numerator from *i* steps before recurrs
            Just i ->
              let il = BI.fromInt i + one
                  finitPartOfAcc = acc `stripNDigitsOnTheRight` il
                  finit = finitPartOfAcc
                        + whole `appendNZerosOnTheRight` (shift - il)
              in  PreciseFloat
                    { finit
                    , infinit: acc - finitPartOfAcc `appendNZerosOnTheRight` il
                    , infinitLength: il
                    , shift
                    }
        -- Numerator is zero -> no recurrence, infinit part equals zero
        | otherwise = PreciseFloat
          { finit: acc + whole `appendNZerosOnTheRight` shift
          , infinit: zero
          , shift
          , infinitLength: zero
          }

toRatio :: PreciseFloat -> Ratio BigInt
toRatio pf@(PreciseFloat pfr)
    | not $ isRecurring pf = Ratio pfr.finit (ten `pow` pfr.shift)
    | otherwise            = Ratio num den
      where
        num = pfr.finit `appendNZerosOnTheRight` pfr.infinitLength
            + pfr.infinit
            - pfr.finit

        den = ten `pow` pfr.shift - ten `pow` (pfr.shift - pfr.infinitLength)

isRecurring :: PreciseFloat -> Boolean
isRecurring (PreciseFloat pfr) = pfr.infinit /= zero

isZero :: PreciseFloat -> Boolean
isZero (PreciseFloat pfr) = pfr.finit == zero && pfr.infinit == zero

scale :: PreciseFloat -> BigInt -> PreciseFloat
scale pf factor = fromRatio $ Ratio (num * factor) den
  where
    (Ratio num den) = toRatio pf


-- Helpers

ten = BI.fromInt 10 :: BigInt

appendNZerosOnTheRight :: BigInt -> BigInt -> BigInt
appendNZerosOnTheRight value shift  = value * (ten `pow` shift)

stripNDigitsOnTheRight :: BigInt -> BigInt -> BigInt
stripNDigitsOnTheRight value shift = value / (ten `pow` shift)

toMixedRatio :: Ratio BigInt -> {whole :: BigInt, propper :: Ratio BigInt}
toMixedRatio impropper@(Ratio num den) = {whole, propper}
  where
    -- Calculate whole part of the impropper ratio and substract it to get
    -- propper ratio
    whole   = num / den
    propper = Ratio (num - whole * den) den
