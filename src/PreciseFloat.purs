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
    {   finit   :: BigInt
    ,   infinit :: BigInt
    ,   shift   :: Int
    }

fromInt :: Int -> Int -> Int -> PreciseFloat
fromInt finit infinit shift =
    PreciseFloat {finit : BI.fromInt finit, infinit : BI.fromInt infinit, shift}

instance showPreciseFloat :: Show PreciseFloat where
    show (PreciseFloat dr) =
        "{finit : " <> toString dr.finit
        <> ", infinit : " <> toString dr.infinit
        <> ", shift : " <> show dr.shift <> "}"

fromRatio :: Ratio BigInt -> PreciseFloat
fromRatio ratio = loop numerator' Nil Nil (-one)
  where
    (Ratio numerator denominator) = norm ratio
    propper = numerator / denominator
    numerator' = numerator - propper * denominator

    loop
      :: BigInt       -- Current divident
      -> List BigInt  -- List of previous dividents
      -> List Char    -- List of whole quotients
      -> Int          -- Counter
      -> PreciseFloat
    loop dividend previousDividends quotients counter
        | dividend == zero =
          let counter' = if counter == (-one) then zero else counter
          in  PreciseFloat
            {   finit   : fromCharList quotients + propper `shiftLeft` (BI.fromInt counter')
            ,   infinit : zero
            ,   shift   : counter'
            }
        | otherwise =
            case dividend `elemIndex` previousDividends of
                -- In case of recurrence, return the result, otherwise, divide
                -- the remaining numerator further
                Just i_infinit ->
                    let i_drop  = length quotients - i_infinit - one
                        infinit = fromCharList $ drop i_drop quotients
                        finit   = fromCharList quotients - infinit
                                + propper `shiftLeft` (BI.fromInt counter)
                    in  PreciseFloat {finit, infinit, shift : counter}
                Nothing ->
                    loop dividend' previousDividends' quotients' counter'
                      where
                        counter' = counter + one
                        previousDividends' = dividend : previousDividends
                        -- Factorize by the current denominator, and save the
                        -- factor to the string of quotients
                        factor = fromBigInt $ dividend / denominator
                        quotients' = quotients <> factor
                        dividend' =  (dividend `mod` denominator) * ten

toRatio :: PreciseFloat -> Ratio BigInt
toRatio pf@(PreciseFloat pfr)
    | not $ isRecurring pf = Ratio pfr.finit (ten `pow` (BI.fromInt pfr.shift))
    | otherwise            = Ratio num       den
  where
    l = countDigits pfr.infinit
    num = (pfr.finit + pfr.infinit) `shiftLeft` l - pfr.finit
    den = (ten `pow` l - one) `shiftLeft` (BI.fromInt pfr.shift)

isRecurring :: PreciseFloat -> Boolean
isRecurring (PreciseFloat pfr) = pfr.infinit /= zero

scale
    :: PreciseFloat                -- Input
    -> BigInt                      -- Scaling factor
    -> Either String PreciseFloat  -- Output
scale pf factor = Right $ fromRatio $ (toRatio pf) * (Ratio factor one)

norm :: Ratio BigInt -> Ratio BigInt
norm r@(Ratio n d)
    | n == zero = Ratio n one
    | d == one  = Ratio n d
    | otherwise = Ratio (n / divisor) (d / divisor)
      where
        divisor = gcd r

-- Helpers

shiftRight :: BigInt -> BigInt -> BigInt
shiftRight a b = a / (ten `pow` b)

shiftLeft :: BigInt -> BigInt -> BigInt
shiftLeft a b  = a * (ten `pow` b)

startswith :: (List Char) -> List Char -> Boolean
startswith (c1 : cs1) (c2 : cs2) | c1 == c2   = startswith cs1 cs2
                                 | otherwise  = false
startswith _          Nil                     = true
startswith Nil        _                       = false

countDigits :: BigInt -> BigInt
countDigits = BI.fromInt <<< String.length <<< toString

ten = BI.fromInt 10 :: BigInt


-- TODO more usefull default
fromCharList :: List Char -> BigInt
fromCharList = (fromMaybe zero)
    <<< fromString
    <<< String.fromCharArray
    <<< Array.fromFoldable

fromBigInt :: BigInt -> List Char
fromBigInt = Array.toUnfoldable
    <<< String.toCharArray
    <<< toString
