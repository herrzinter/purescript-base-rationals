module PreciseFloat
  ( PreciseFloat (..)
  , fromRatio
  , fromInts
  , toRatio
  , isZero
  , isRecurring
  , scale
  -- Helpers
  , appendNZerosOnTheRight
  , stripNDigitsOnTheRight
  , toMixedRatio
  ) where


import Prelude
import PreciseRational

import Data.String as String
import Data.BigInt as BI

import Data.BigInt (BigInt(..), pow)
import Data.Ratio (Ratio(..), (%))
import Data.List (List(..), elemIndex, (:))
import Data.Maybe (Maybe(..))


-- | The `PreciseFloat` data type is a non-fractional representation of
-- | rational numbers, ie. it is an infinit precision floating point number.
-- | Rational numbers can have recurring, and thus, infinit, non-fractional
-- | represenations. The `finit`, ie. not recurring, and `infinit` , ie.
-- | recurring, part are seperated.
-- | `infinitLength` specifies the length of the infinit part. This is
-- | necessarry to encode the difference between infinit parts which are lead by
-- | zeros and infinit parts which are not, eg.
-- | "0.[1]" and "0.[001]" have both infinit = 1, but different
-- | `infinitLength`s.
-- | `shift` specifies by how many digits the radix points has to be shifted to
-- | the left, so that, `finit` and `infinit` become whole numbers. Thus,
-- | in contrast to traditional floating point types, shift is only positive,
-- | and always interpreted as negative exponent, eg.
-- | "1000" is coded as {f: 1000, i: 0, il: 0, s: 0},
-- | but "0.001" as {f: 1, i: 0, il: 0, s: 3}
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

-- | Convert a `PreciseRational` to a `PreciseFloat`, ie. convert a fractional
-- | representation of a rational to a non-fractoinal one.
fromRatio :: PreciseRational -> PreciseFloat
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

-- | Construct `PreciseFloat` from four `Int`s describiing `finit`, `infinit`,
-- | `infinitLength` and `shift`
fromInts :: Int -> Int -> Int -> Int -> PreciseFloat
fromInts finit infinit infinitLength shift = PreciseFloat
  { finit:          BI.fromInt finit
  , infinit:        BI.fromInt infinit
  , infinitLength:  BI.fromInt infinitLength
  , shift:          BI.fromInt shift
  }

-- | Convert a `PreciseFloat` to a `PreciseRational` ie. convert a
-- | non-fractional representation of a rational to a fractional one.
toRatio :: PreciseFloat -> PreciseRational
toRatio pf@(PreciseFloat pfr)
    | not $ isRecurring pf = pfr.finit % (ten `pow` pfr.shift)
    | otherwise            = num % den
      where
        num = pfr.finit `appendNZerosOnTheRight` pfr.infinitLength
            + pfr.infinit
            - pfr.finit

        den = ten `pow` pfr.shift - ten `pow` (pfr.shift - pfr.infinitLength)

-- | Check if the `PreciseFloat` has a recurring part
isRecurring :: PreciseFloat -> Boolean
isRecurring (PreciseFloat pfr) = pfr.infinit /= zero

-- | Check if the `PreciseFloat` is zero
isZero :: PreciseFloat -> Boolean
isZero (PreciseFloat pfr) = pfr.finit == zero && pfr.infinit == zero

-- | Scale the `PreciseFloat` by a `BigInt` `factor`
scale :: PreciseFloat -> BigInt -> PreciseFloat
scale pf factor = fromRatio ((num * factor) % den)
  where
    (Ratio num den) = toRatio pf


-- Helpers

ten = BI.fromInt 10 :: BigInt

-- | Eg. 123 `appendNZerosOnTheRight` 2 -> 12300
appendNZerosOnTheRight :: BigInt -> BigInt -> BigInt
appendNZerosOnTheRight value shift  = value * (ten `pow` shift)

-- | eg. 12345 `stripNDigitsOnTheRight` 2 -> 123
stripNDigitsOnTheRight :: BigInt -> BigInt -> BigInt
stripNDigitsOnTheRight value shift = value / (ten `pow` shift)

-- | Seperate the `whole` from the `propper` part of an (possibly) impropper
-- | fraction
toMixedRatio :: PreciseRational -> {whole :: BigInt, propper :: PreciseRational}
toMixedRatio impropper@(Ratio num den) = {whole, propper}
  where
    -- Calculate whole part of the impropper ratio and substract it to get
    -- propper ratio
    whole   = num / den
    propper = (num - whole * den) % den
