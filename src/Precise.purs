module Precise where


import Prelude
import Data.String as String
import Data.Array as Array

import Data.BigInt (BigInt(..), fromInt, fromString, pow, toString)
import Data.Ratio (Ratio(..), denominator, numerator)
import Data.List (List(..), elemIndex, drop, dropWhile, length, reverse, snoc, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either (..))
import Control.Error.Util (note)


data PseudoFloat = PseudoFloat
    {   finit   :: BigInt
    ,   infinit :: BigInt
    ,   shift   :: Int
    }

pfFromInt finit infinit shift = PseudoFloat
    {   finit   : fromInt finit
    ,   infinit : fromInt infinit
    ,   shift   : shift
    }

instance showPseudoFloat :: Show PseudoFloat where
    show (PseudoFloat dr) =
        "{finit : " <> toString dr.finit
        <> ", infinit : " <> toString dr.infinit
        <> ", shift : " <> show dr.shift <> "}"

fromRatio :: Ratio BigInt -> PseudoFloat
fromRatio (Ratio numerator denominator) = loop numerator Nil Nil (-one)
  where
    loop :: BigInt       -- Current divident
           -> List BigInt  -- List of previous dividents
           -> List Char    -- List of whole quotients
           -> Int          -- Counter
           -> PseudoFloat
    loop dividend previousDividends quotients counter
        | dividend == zero = PseudoFloat
            {   finit   : fromCharList quotients
            ,   infinit : zero
            ,   shift   : counter
            }
        | otherwise =
            case dividend `elemIndex` previousDividends of
                -- In case of recurrence, return the result, otherwise, divide
                -- the remaining numerator further
                Just i_infinit ->
                    let i_drop  = length quotients - i_infinit - one
                        infinit = fromCharList $ drop i_drop quotients
                        finit   = fromCharList quotients - infinit
                    in  PseudoFloat {finit, infinit, shift : counter}
                Nothing ->
                    loop dividend' previousDividends' quotients' counter'
                      where
                        counter' = counter + one
                        previousDividends' = dividend : previousDividends
                        -- Factorize by the current denominator, and save the
                        -- factor to the string of quotients
                        factor = fromBigInt $ dividend / denominator
                        quotients' = quotients <> factor
                        dividend' = (dividend `mod` denominator) * ten


isRecurring :: PseudoFloat -> Boolean
isRecurring (PseudoFloat pseudoFloatRec) = pseudoFloatRec.infinit /= zero

scale
    :: PseudoFloat                -- Input
    -> BigInt                     -- Scaling factor
    -> Either String PseudoFloat  -- Output
scale pf@(PseudoFloat pfr) factor
    | not $ isRecurring pf = pure $ PseudoFloat pfr {finit = pfr.finit * factor}
    | otherwise            = do
        let {factor', shift'} = splitShift factor pfr.shift

        let value = scaleUntilRecurring pfr.finit pfr.infinit factor'

        {finit, infinit} <- note "No recurrence" (splitTrailingRecurrence value)

        pure $ PseudoFloat {finit, infinit, shift : shift'}


-- Helpers

scaleUntilRecurring :: BigInt -> BigInt -> BigInt -> BigInt
scaleUntilRecurring finit infinit factor = loop one v0
  where
    l   = countDigits infinit
    v0  = ((finit + infinit) * factor) `shiftLeft` l + infinit * factor

    loop i val =
        if    val' `shiftRight` (l * i) == val `shiftRight` (l * (i - one))
        then  val' `shiftRight` (l * i)
        else  loop (i + one) val'
      where
        val' = val `shiftLeft` l + infinit * factor

-- Remove tailing zeros of *factor* and adjust *shift*. These zeros
-- only change the *shift* of the *PseudoFloat*, but are not captured
-- by the rest of the algorithm, so they're removed up front
splitShift :: BigInt -> Int -> {factor' :: BigInt, shift' :: Int}
splitShift factor shift = {factor', shift'}
  where
    factorChars = reverse $ fromBigInt factor
    factorChars' = dropWhile ((==) '0') factorChars
    factor' = fromCharList $ reverse factorChars'
    shift' = shift - (length factorChars - length factorChars')


splitTrailingRecurrence :: BigInt -> Maybe {finit :: BigInt, infinit :: BigInt}
splitTrailingRecurrence int = do
    let chars = reverse $ fromBigInt int

    infinitChars <- loop chars Nil Nil

    let finit' = fromCharList <<< reverse $ drop (length infinitChars * 2) chars
    let finit = finit' * ten `pow` (fromInt $ length infinitChars)
    let infinit = fromCharList $ reverse infinitChars

    pure {finit, infinit}

  where
    loop (c : cs) acc can = loop cs acc' can'
      where
        acc' = acc `snoc` c
        can' = if cs `startswith` acc' then acc' else can
    loop _        _   Nil = Nothing
    loop _        _   can = Just can


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
countDigits = fromInt <<< String.length <<< toString

ten = fromInt 10 :: BigInt


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
