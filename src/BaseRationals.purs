-- | Implements in arbitrary basis given arbitrary digits:
-- | * parsing a string for a rational
-- | * rendering a non-fractional string represenation of a rational
-- |
-- | Digits can be created from an `Array` of `Char`s.
-- | ```
-- | let digits  = digitsFromArray ['0', '1', '2', 'A', 'B']
-- | ```
-- |
-- | `toString` and `fromString` run both in the `(Either String)` monad,
-- | providing `String` error messages.
-- | Both have `Digits` and a *basis* as `Int` as their first two arguments;
-- | and a `String` or a `PreciseRational` as third one respectively. A usage
-- | example is:
-- | ```
-- | string :: Either String String
-- | string = do
-- |     let pr = PR.fromInts 1 7 :: PreciseRational
-- |     let basis = 4
-- |     s <-
-- |     pure s
-- |
-- | pr :: Either String PreciseRational
-- | pr = do
-- |     let s = "A2AB01.20B1A" :: String
-- |     let basis = 5
-- |     pr <- fromString digits basis s
-- | ```

module BaseRationals
  ( Digits
  , digitsFromArray
  , arrayFromDigits
  , maximalBasisOfDigits

  , fromString
  , toString

  , index
  , digitIndex
  ) where


import PreciseRational
import PreciseFloat
import Prelude

import Data.Int as Int
import Data.BigInt as BI
import Data.String as String
import Data.Array as Array
import Data.List as List

import Data.EuclideanRing (class EuclideanRing)
import Data.BigInt (BigInt(..), pow, toNumber, abs)
import Data.Ratio (Ratio(..), (%))
import Data.Foldable (any, foldl)
import Data.List (List(..), length, init, take, drop, filter,reverse, (:), (..),
                  elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Control.Error.Util (note)
import Control.Monad.Rec.Class (Step(..), tailRecM3)
import Control.MonadPlus (guard)
import Control.MonadZero (class MonadZero)


-- | Container type for an `Array` of `Chars` representing digits. The
-- | constructor is hidden, as digits are more constrained than an `Array` of
-- | `Char`s, use `digitsFromArray` instead.
data Digits = Digits (Array Char)

instance showDigits :: Show Digits where
    show (Digits array) = show array

-- | Wrap `Array` of `Char`s in digit container, if array
-- | * contains at least two digits, as the minimum basis is two
-- | * all contained digits need to be unique
digitsFromArray :: Array Char -> Either String Digits
digitsFromArray array = do
    unless
        (Array.length array >= 2)
        (Left $ "Could not create digits " <> show array
            <> " has less than two digits")
    unless
        (hasNoRepeatingElem $ List.fromFoldable array)
        (Left $ "Array " <> show array <> " has repeating digits")
    pure $ Digits array

-- | Unwrap `Array` of `Char`s from `Digits` container
arrayFromDigits :: Digits -> Array Char
arrayFromDigits (Digits array) = array

-- | Get the maximal possible basis for `Digits`. It equals the length of the
-- | wrapped `Array` of `Char`s
maximalBasisOfDigits :: Digits -> Int
maximalBasisOfDigits (Digits array) = Array.length array

-- | Parse a `PreciseRational` from a `String` in basis `Int` given
-- | `Digits`.
fromString :: Digits -> Int -> String -> Either String PreciseRational
fromString digits basis string = do
    errorUnlessValidBasis basis digits

    let cs0 = List.fromFoldable $ String.toCharArray $ string

    let {sign, cs: cs1} = splitSign cs0
    {fcs: fcs0, ics} <- splitFinitAndInfinit cs1
    let {shift, cs: fcs1} = splitShift fcs0

    let basisBI = BI.fromInt basis
    finit <- biFromCharList digits basisBI fcs1
    infinit <- biFromCharList digits basisBI ics

    let ratio =
          if infinit == zero
          then finit % one
          else (finit * factor + infinit) % factor
            where
              infinitLength = BI.fromInt $ List.length ics
              factor = basisBI `pow` infinitLength - one

    pure $ ratio * (sign % (basisBI `pow` shift))

-- | Render a non-fractional `String`-representation of a `PreciseRational`
-- | in basis `Int` given `Digits`.
toString :: Digits -> Int -> PreciseRational -> Either String String
toString digits basis ratio = do
    errorUnlessValidBasis basis digits

    let basisBI = BI.fromInt basis
    -- Seperate the *whole* part of the fraction and the *propper*
    let {whole, propper} = toMixedRatio ratio
    -- Get *pre* and *post* radix chars
    pre   <- preFromWhole    digits basisBI whole
    post  <- postFromPropper digits basisBI (fromRatio propper)

    let cs =  pre <> ('.' : Nil) <> post
    cs' <- note "String is empty" (alterCharsForDisplay cs)

    pure $ String.fromCharArray $ List.toUnfoldable $ cs'


-- | Lookup the *digit* `Char` with *index* `BigInt` in `Digits`
index :: Digits -> BigInt -> Either String Char
index digits iBI = do
    let digitArray = arrayFromDigits digits
    i <- note
        ("Failed to convert BigInt index " <> BI.toString iBI <> " to Int")
        (Int.fromNumber $ toNumber iBI)
    c <- note
        ("Failed to lookup index " <> show i <> " in " <> show digits)
        (digitArray `Array.index` i)
    pure c

-- | Lookup the *index* `BigInt` of the first occurence of `Char` in
-- | `Digits`
digitIndex :: Char -> Digits -> Either String BigInt
digitIndex c digits = do
    let digitArray = arrayFromDigits digits
    i <- note
        ("Failed to lookup " <> show c <> " in digits " <> show digits)
        (c `Array.elemIndex` digitArray)
    pure $ BI.fromInt i


--
--  Helpers
--

-- Check if at least one of the elements of the list occur twice in the list
hasNoRepeatingElem :: forall e . Eq e => List e -> Boolean
hasNoRepeatingElem list = loop list Nil
  where
    loop (e : es) es' | not $ e `elem` es'  = loop es (e : es')
                      | otherwise           = false
    loop _        _                         = true

-- Unless guard, checking if the current basis is in the range of valid
-- basis, ie. if `2 <= basis <= maximalBasis`
errorUnlessValidBasis :: Int -> Digits -> Either String Unit
errorUnlessValidBasis basis digits = do
    let maximalBasis = maximalBasisOfDigits digits
    unless
        (basis >= 2)
        (Left $ "Basis " <> show basis <> " smaller than '2'")
    unless
        (basis <= maximalBasis)
        (Left $ "Basis " <> show basis <> " bigger then maximal basis "
                         <> show maximalBasis)

-- Parse a `BigInt` from characters given *digits* and *basis*
biFromCharList
    :: Digits               -- Digits
    -> BigInt               -- Basis
    -> List Char            -- Input characters
    -> Either String BigInt -- Error or parsed number
biFromCharList digits basis cs0 = loop (reverse cs0) zero zero
  where
    loop (c : cs) accumulator position  = do
        bi <- c `digitIndex` digits
        let positionValue = basis `pow` position
        let delta         = bi * positionValue

        loop cs (accumulator + delta) (position + one)
    loop  _       accumulator _         = pure accumulator

-- Render character representatoin of a whole number given *digits* and *basis*
preFromWhole
    :: Digits                     -- Digits
    -> BigInt                     -- Basis
    -> BigInt                     -- Whole number
    -> Either String (List Char)  -- Error or pre radix characters
preFromWhole digits basis whole = loop Nil whole
  where
    loop cs dividend
      | dividend >= one = do
          -- Calculate quotient and remainder of division by
          -- basis
          let remainder = dividend `mod` basis
          let quotient = (dividend - remainder) / basis
          -- Get Corresponding digit character
          c <- digits `index` remainder

          loop (c : cs) quotient
      | otherwise = Right cs

-- Render character representation of a propper fraction in a non-fractional
-- representation given *digits* and *basis*
postFromPropper
    :: Digits                     -- Digits
    -> BigInt                     -- Base
    -> PreciseFloat               -- Remainder
    -> Either String (List Char)  -- Error or post radix characters
postFromPropper digits basis pf0 = tailRecM3 loop Nil Nil (pf0 `scale` basis)
  where
    loop
        :: List PreciseFloat  -- Intermediate values to check for reccurence
        -> List Char          -- Accumulator for the output characters
        -> PreciseFloat       -- Intermediate value
        -> Either String _
    loop pfs cs pf@(PreciseFloat pfr)
        | not $ isZero pf = case pf `List.elemIndex` pfs  of
            Nothing -> do
                -- Calculate index *i* and lookup corresponding char *c*
                let n = pfr.shift - pfr.infinitLength
                let iBI = pfr.finit `stripNDigitsOnTheRight` n
                c <- digits `index` iBI
                let finit' = pfr.finit - iBI `appendNZerosOnTheRight` n

                pure $ Loop
                    { a: (pf : pfs)
                    , b: (c : cs)
                    , c: (PreciseFloat pfr {finit = finit'}) `scale` basis
                    }
            -- Recurrence -> return with parantheses marking recurrence
            Just i ->
                let i' = length pfs - i - one
                    cs' = reverse cs
                    finitChars = take i' cs'
                    infinitChars = ('[' : Nil) <> (drop i' cs') <> (']' : Nil)
                in  pure $ Done (finitChars <> infinitChars)
        | otherwise = pure $ Done $ reverse cs

-- Split a trailing sign from a list of characters, and return sign and
-- remaining chars
splitSign :: List Char -> {sign :: BigInt, cs :: List Char}
splitSign ('-' : cs)  = {sign: (-one), cs}
splitSign cs          = {sign:   one , cs}

-- Remove the radix point from a character representatoin of a number and
-- calculate the corresponding shift, eg.
-- "123.45" -> {shift: 2, cs: "12345"}
splitShift :: List Char -> {shift :: BigInt, cs :: List Char}
splitShift cs = {shift, cs : filter (\c -> c /= '.') cs}
  where
    -- Calculate shift from position of radix point
    indexOfRadixPoint = case '.' `List.elemIndex` cs of
        Just i  -> i + one
        Nothing -> length cs
    shift = BI.fromInt (length cs - indexOfRadixPoint)

-- TODO this should be implemented via a parser library
splitFinitAndInfinit
    :: List Char
    -> Either String {fcs:: List Char, ics:: List Char}
splitFinitAndInfinit cs = f ('[' `List.elemIndex` cs) (']' `List.elemIndex` cs)
  where
    f (Just iOpenBracket) Nothing               = Left "Missing ']'"
    f Nothing             (Just iCloseBracket)  = Left "Missing '['"
    f Nothing             Nothing               = Right {fcs: cs, ics: Nil}
    f (Just iOpenBracket) (Just iCloseBracket)  = do
        -- Check if '[' and ']' are used correctly
        unless
            (not $ 1 < (List.length $ filter (\c -> c == '[') cs))
            (Left "More than one '[' present")
        unless
            (not $ 1 < (List.length $ filter (\c -> c == ']') cs))
            (Left "More than one ']' present")
        unless
            (iOpenBracket < iCloseBracket)
            (Left "Recurrence brackets are in wrong order")
        unless
            (not $ iOpenBracket + 1 == iCloseBracket)
            (Left "Recurrence brackets are empty")

        iPoint <- note
            "Recurrence brackets present, but no radix point"
            ('.' `List.elemIndex` cs)

        unless
            (iPoint < iOpenBracket)
            (Left "Recurrence brackets appear before the radix point")
        unless
            (iCloseBracket == (List.length cs - 1))
            (Left "']' has to be last character")

        let fcs = take iOpenBracket cs
        ics <- note
            "Seperating reuccring characters failed"
            (init $ drop (iOpenBracket + one) cs)

        pure {fcs, ics}

-- Add/remove some characters to display number more naturally, eg.
-- "123.0" -> Just "123"
alterCharsForDisplay :: List Char -> Maybe (List Char)
alterCharsForDisplay cs = do
    p <- '.' `List.elemIndex` cs
    let len = length cs
    case Nothing of
        _ | p == zero && len == one -> Just $ Cons '0' Nil  -- "." -> "0"
          | p == zero               -> Just $ '0' : cs      -- ".x" -> "0.x"
          | p == len - one          -> init cs              -- "x." -> "x"
          | otherwise               -> Just cs              -- Do nothing
