
module Basis
  ( isValidDigitArray
  , createIsFinitFunction
  , createConversionFunctions
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
import Data.BigInt (BigInt(..), fromString, pow, toNumber, toString, abs)
import Data.Ratio (Ratio(..))
import Data.Foldable (any, foldl)
import Data.List (List(..), length, init, take, drop, filter, elemIndex, index,
                  reverse, (:), (..), elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Control.Error.Util (note)
import Control.Monad.Rec.Class (Step(..), tailRecM3)
import Control.MonadPlus (guard)

-- | Is `digitArray` a valid array of digits? It is, if it contains at least
-- | two digits, as the minimal valid basis is two, and all digits are distinct
isValidDigitArray :: Array Char -> Boolean
isValidDigitArray digits =
    Array.length digits >= 2 && hasNoRepeatingElem (List.fromFoldable digits)

createIsFinitFunction
    :: Array Char
    -> Maybe (Int -> PreciseRational -> Either String Boolean)
createIsFinitFunction digitArray = do
    guardValidDigitArray digitArray
    pure isFinit
  where
    digits = List.fromFoldable digitArray
    maximalBasis = Array.length digitArray

    -- The prime factors of all basis are needed to compute if a fraction
    -- has a finit non-fractional representation. As computation of primes
    -- and factorizations is expensive, it is done once for all possible basis
    listOfPrimeFactorLists :: List (List BigInt)
    listOfPrimeFactorLists = do
        basis <- 2 .. maximalBasis
        let {factors} = factorize primes basis
        pure $ map BI.fromInt factors
      where
        primes = calculatePrimes maximalBasis

    getPrimeFactorsOfBasis :: Int -> Either String (List BigInt)
    getPrimeFactorsOfBasis basis = do
        errorUnlessValidBasis basis maximalBasis
        -- The first valid basis is 2 and thus, has index zero. Therefore, the
        -- basis is shifted by two to get the corresponding index
        let basisIndex = basis - 2
        primeFactors <- note
            ("Could not get prime factors for basis " <> show basis)
            (listOfPrimeFactorLists `index` basisIndex)
        pure $ primeFactors

    -- | Is the non-fractional representation of `Ratio` finit in `basis`?
    isFinit :: Int -> PreciseRational -> Either String Boolean
    isFinit basis (Ratio _ den) = do
        errorUnlessValidBasis basis maximalBasis
        primeFactors <- getPrimeFactorsOfBasis basis
        -- If the `den` can be completely factorized by the `primeFactors` of
        -- `basis`, the fold results in one. In this case, the non-fractional
        -- representation of `Ratio` is finit in `basis`
        pure (foldl factorizeMany den primeFactors == one)
      where
        factorizeMany num factor
            | num `mod` factor == zero = factorizeMany (num / factor) factor
            | otherwise                = num

createConversionFunctions
    :: Array Char
    -> Maybe
          { fromString  :: Int -> String -> Either String (PreciseRational)
          , toString    :: Int -> PreciseRational -> Either String String
          }
createConversionFunctions digitArray = do
    guardValidDigitArray digitArray
    pure $ {fromString, toString}
  where
    digits = List.fromFoldable digitArray
    maximalBasis = Array.length digitArray

    fromString :: Int -> String -> Either String (PreciseRational)
    fromString basis string = do
        errorUnlessValidBasis basis maximalBasis

        let cs0 = List.fromFoldable $ String.toCharArray $ string

        let {sign, cs: cs1} = splitSign cs0
        let {shift, cs: cs2} = splitShift cs1

        let basisBI = BI.fromInt basis
        numerator <- biFromCharList digits basisBI cs2
        pure $ Ratio (sign * numerator) (basisBI `pow` shift)

    toString :: Int -> PreciseRational -> Either String String
    toString basis ratio = do
        errorUnlessValidBasis basis maximalBasis

        let basisBI = BI.fromInt basis
        -- Seperate the *whole* part of the fraction and the *propper*
        let {whole, propper} = toMixedRatio ratio
        -- Get *pre* and *post* radix chars
        pre   <- preFromWhole    digits basisBI whole
        post  <- postFromPropper digits basisBI (fromRatio propper)

        let cs =  pre <> ('.' : Nil) <> post
        cs' <- note "String is empty" (alterCharsForDisplay cs)

        pure $ String.fromCharArray $ List.toUnfoldable $ cs'

-- Helpers

-- | Convert a list of characters to big integer given a list of digits and a
-- basis
biFromCharList
    :: List Char            -- Digits
    -> BigInt               -- Basis
    -> List Char            -- Input characters
    -> Either String BigInt -- Error or parsed number
biFromCharList digits basis cs0 = loop (reverse cs0) zero zero
  where
    loop (c : cs) accumulator position  = do
        digitValue <- note
            ("Failed to lookup " <> show c <> " in digits " <> show digits)
            (c `elemIndex` digits)

        let positionValue = basis `pow` position
        let delta         = (BI.fromInt digitValue) * positionValue

        loop cs (accumulator + delta) (position + one)
    loop  _       accumulator _         = pure accumulator

-- | Lookup a character in a list of characters identified by an BigInt index
biIndex :: List Char -> BigInt -> Either String Char
biIndex digits iBI = do
    i <- note
        ("Failed to convert BigInt index " <> BI.toString iBI <> " to Int")
        (Int.fromNumber $ toNumber iBI)
    c <- note
        ("Failed to lookup index " <> show i <> " in " <> show digits)
        (digits `index` i)
    pure c

preFromWhole
    :: List Char -- Digits
    -> BigInt    -- Basis
    -> BigInt    -- Whole number
    -> Either String (List Char)
preFromWhole digits basis whole = loop Nil whole
  where
    loop cs dividend
      | dividend >= one = do
          -- Calculate quotient and remainder of division by
          -- basis
          let remainder = dividend `mod` basis
          let quotient = (dividend - remainder) / basis
          -- Get Corresponding digit character
          c <- digits `biIndex` remainder

          loop (c : cs) quotient
      | otherwise = Right cs

postFromPropper
    :: List Char                  -- Digits
    -> BigInt                     -- Base
    -> PreciseFloat               -- Remainder
    -> Either String (List Char)  -- Post radix string
postFromPropper digits basis pf0 = tailRecM3 loop Nil Nil (pf0 `scale` basis)
  where
    loop
        :: List PreciseFloat  -- Intermediate values to check for reccurence
        -> List Char          -- Accumulator for the output characters
        -> PreciseFloat       -- Intermediate value
        -> Either String _
    loop pfs cs pf@(PreciseFloat pfr)
        | not $ isZero pf = case pf `elemIndex` pfs  of
            Nothing -> do
                -- Calculate index *i* and lookup corresponding char *c*
                let n = pfr.shift - pfr.infinitLength
                let iBI = pfr.finit `stripNDigitsOnTheRight` n
                c <- digits `biIndex` iBI
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

-- | Split a trailing sign from a list of characters, and return sign and
-- | remaining chars
splitSign :: List Char -> {sign :: BigInt, cs :: List Char}
splitSign ('-' : cs)  = {sign: (-one), cs}
splitSign cs          = {sign:   one , cs}

-- | Remove the radix point from a character representatoin of a number and
-- | calculate the corresponding shift, eg. "123.45" -> {shift: 2, cs: "12345"}
splitShift :: List Char -> {shift :: BigInt, cs :: List Char}
splitShift cs = {shift, cs : filter (\c -> c /= '.') cs}
  where
    -- Calculate shift from position of radix point
    indexOfRadixPoint = case '.' `elemIndex` cs of
        Just i  -> i + one
        Nothing -> length cs
    shift = BI.fromInt (length cs - indexOfRadixPoint)

alterCharsForDisplay :: List Char -> Maybe (List Char)
alterCharsForDisplay cs = do
    p <- '.' `elemIndex` cs
    let len = length cs

    case Nothing of
        _ | p == zero && len == one -> Just $ Cons '0' Nil
          | p == zero               -> Just $ '0' : cs
          | p == len - one          -> init cs
          | otherwise               -> Just cs


-- | Factorize a member of an euclidian ring by a list of factors
factorize
    :: forall n . EuclideanRing n => Eq n =>
    List n -> n -> {factors :: List n, remainder :: n}
factorize factors number
    | number /= zero =
        let factorizeRecursive (f : fs) factorization
                | factorization.remainder `mod` f == zero =
                    factorizeRecursive (f : fs) factorization'
                      where
                        factorization' =
                          { factors: (f : factorization.factors)
                          , remainder: (factorization.remainder / f)
                          }
                | otherwise =
                factorizeRecursive fs factorization
            factorizeRecursive _ factorization = factorization
        in factorizeRecursive factors {factors : Nil, remainder : number}
    | otherwise = {factors : Nil, remainder : zero}


-- | Calculate all prime numbers between two and the maximum
calculatePrimes
    :: forall n . EuclideanRing n => Eq n => Ord n
    => n -> List n
calculatePrimes maximum
    | maximum > one =
        let calculatePrimes' number primes
                | number > maximum = primes
                | any (\p -> number `mod` p == zero) primes =
                    calculatePrimes' (number + one) primes
                | otherwise =
                    calculatePrimes' (number + one) (number : primes)
        in calculatePrimes' (one + one) Nil
    | otherwise = Nil


hasNoRepeatingElem :: forall e . Eq e => List e -> Boolean
hasNoRepeatingElem list = loop list Nil
  where
    loop (e : es) es' | not $ e `elem` es'  = loop es (e : es')
                      | otherwise           = false
    loop _        _
           = true

guardValidDigitArray digitArray = do
    guard $ isValidDigitArray digitArray

-- Unless guard, checking if the current basis is in the range of valid
-- basis, ie. if `2 <= basis <= maximalBasis`
errorUnlessValidBasis basis maximalBasis = do
    unless
        (basis >= 2)
        (Left $ "Basis " <> show basis <> " smaller than '2'")
    unless
        (basis <= maximalBasis)
        (Left $ "Basis " <> show basis <> " bigger then maximal basis "
                         <> show maximalBasis)
