module Basis where


import PreciseFloat
import Prelude

import Data.Int as Int
import Data.BigInt as BI
import Data.BigInt (BigInt(..), fromString, pow, toNumber, toString, abs)
import Data.Ratio (Ratio(..), denominator, numerator)
import Math (remainder)
import Data.EuclideanRing (class EuclideanRing)
import Data.Foldable (any, foldl)
import Data.String as String
import Data.Array as Array
import Data.List as List
import Data.List (List(..), length, init, take, drop, filter, elemIndex, index,
                  reverse, (:), (..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Control.Error.Util (note)
import Control.Monad.Rec.Class (Step(..), tailRecM3)


type BasisFunctions =
    {   isFinit     :: Int -> Ratio BigInt -> Maybe Boolean
    ,   fromString  :: Int -> String       -> Either String (Ratio BigInt)
    ,   toString    :: Int -> Ratio BigInt -> Either String String
    }

-- Basis smaller equal one do not make sense
isValidDigitArray :: Array Char -> Boolean
isValidDigitArray digitArray = Array.length digitArray >= 2

createBasisFunctions :: Array Char -> Maybe BasisFunctions
createBasisFunctions digitsArray
    | not $ isValidDigitArray digitsArray = Nothing
    | otherwise                           = Just basisFunctions
  where
    digits = List.fromFoldable digitsArray
    basisMax = length digits

    -- The prime factorizations of all possible basis of a list of digits
    -- is used for several calculations eg. checking, if a fraction has a
    -- finit representation in a certain basis. As it is expensive to
    -- calculate primes and prime factorizations their are calculated for
    -- one module

    primeFactorsList :: List (List BigInt)
    primeFactorsList = do
        basis <- 2 .. basisMax
        let factorization = factorize primes basis
        pure $ map BI.fromInt factorization.factors
      where
        primes = calculatePrimes basisMax

    getPrimeFactors basis = primeFactorsList `index` (basis - 2)

    isFinit basis (Ratio _ denominator)
        | basis > basisMax = Nothing
        -- If the denominator can be complete factorized by the primefactors
        -- of the current basis, the non-fractional rendering of the
        -- rational is finit
        | otherwise = Just (foldl factorizeMany denominator primeFactors == one)
          where
            primeFactors = fromMaybe Nil (getPrimeFactors basis)

            factorizeMany number factor
                | number `mod` factor == zero = factorizeMany (number / factor) factor
                | otherwise                   = number

    fromString :: Int -> String -> Either String (Ratio BigInt)
    fromString basis =
        fromCharList basis <<< List.fromFoldable <<< String.toCharArray

    -- Match possible negative sign, parse remaining chars and negate result
    fromCharList :: Int -> List Char -> Either String (Ratio BigInt)
    fromCharList basis ('-' : cs) = do
        ratio <- fromCharList basis cs
        pure (-ratio)
    fromCharList basis cs
        | 1 < basis && basis <= basisMax = do
            let point = case '.' `elemIndex` cs of
                    Just i  -> i + one
                    Nothing -> length cs
            let shift = BI.fromInt (length cs - point)
            let cs' = filter (\c -> c /= '.') cs

            numerator <- parseDigits digits basisBI cs'
            let denominator = basisBI `pow` shift

            pure $ Ratio numerator denominator
          where
            basisBI = BI.fromInt basis
        | otherwise = Left $ "Basis not between 1 and " <> show basisMax

    toString :: Int -> Ratio BigInt ->  Either String String
    toString basis ratio@(Ratio numerator denominator)
        | basis > basisMax = Left $ "Basis exceeds " <> show basis
        | otherwise = do
            -- Seperate the *propper* part of the fraction and the
            -- *remainder*
            let propper = abs $ numerator / denominator
            let remainder = ratio - (Ratio propper one)

            -- Calculate *pre* and *post* radix chars
            pre <- stringFromBase Nil propper
            post <- getPost digits basisBI (fromRatio remainder)
            let string = pre <> (Cons '.' Nil) <> post

            -- TODO Alter chars for display
            string' <- note "String is empty" (cleanString string)

            pure <<< String.fromCharArray <<< List.toUnfoldable $ string'
          where
            basisBI = BI.fromInt basis
            -- Calculate a string representation of `dividend` in `basis`
            stringFromBase :: List Char -- Accumulator for output string
                           -> BigInt    -- Accumulator for number
                           -> Either String (List Char)
            stringFromBase cs dividend
                | dividend >= one = do
                    -- Calculate quotient and remainder of division by
                    -- basis
                    let remainder = dividend `mod` basisBI
                    let quotient = (dividend - remainder) / basisBI
                    -- Get Corresponding digit character
                    c <- lookupDigits digits remainder

                    stringFromBase (c : cs) quotient
                | otherwise = Right cs

            cleanString string = do
                p <- '.' `elemIndex` string
                let len = length string

                case Nothing of
                    _ | p == zero && len == one -> Just $ Cons '0' Nil
                    _ | p == zero               -> Just $ '0' : string
                      | p == len - one          -> init string
                      | otherwise               -> Just string

    basisFunctions =    {   fromString  : fromString
                        ,   toString    : toString
                        ,   isFinit     : isFinit
                        }

parseDigits
    :: List Char            -- Digits
    -> BigInt               -- Basis
    -> List Char            -- Input characters
    -> Either String BigInt -- Error or parsed number
parseDigits digits basis cs0 = loop (reverse cs0) zero zero
  where
    loop (c : cs) accumulator position  = do
        digitValue <- note
            ("Failed to lookup " <> show c <> " in digits " <> show digits)
            (c `elemIndex` digits)

        let positionValue = basis `pow` position
        let delta         = (BI.fromInt digitValue) * positionValue

        loop cs (accumulator + delta) (position + one)
    loop  _       accumulator _         = pure accumulator

getPost
    :: List Char                  -- Digits
    -> BigInt                     -- Base
    -> PreciseFloat               -- Remainder
    -> Either String (List Char)  -- Post radix string
getPost digits basis pf0 = tailRecM3 loop Nil Nil (pf0 `scale` basis)
  where
    loop
        :: List PreciseFloat  -- Intermediate values to check for reccurence
        -> List Char          -- Accumulator for the output characters
        -> PreciseFloat       -- Intermediate value
        -> Either String _
    loop pfs cs pf@(PreciseFloat pfr)
        | combineParts pf /= zero = case pf `elemIndex` pfs  of
            Nothing -> do
                -- Calculate index *i* and lookup corresponding char *c*
                let shift = one `shiftLeft` (pfr.shift - pfr.infinitLength)
                let iBI = pfr.finit / shift
                c <- lookupDigits digits iBI

                -- Update finit part according to index *i*
                let pfr' = pfr {finit = pfr.finit - iBI * shift}

                pure $ Loop $ {   a: (pf : pfs)
                              ,   b: (c : cs)
                              ,   c: (PreciseFloat pfr') `scale` basis
                              }
            -- Recurrence -> return with parantheses marking recurrence
            Just i ->
                let i' = length pfs - i - one
                    cs' = reverse cs
                    finitChars = take i' cs'
                    infinitChars = ('[' : Nil) <> (drop i' cs') <> (']' : Nil)
                in  pure $ Done (finitChars <> infinitChars)
        | otherwise = pure $ Done $ reverse cs

lookupDigits :: List Char -> BigInt -> Either String Char
lookupDigits digits iBI = do
    i <- note "Failed to convert numbers" (Int.fromNumber $ toNumber iBI)
    c <- note "Failed to lookup character" (digits `index` i)
    pure c

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
                            {   factors     : (f : factorization.factors)
                            ,   remainder   : (factorization.remainder / f)
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
