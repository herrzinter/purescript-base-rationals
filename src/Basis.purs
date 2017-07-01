module Basis where


import Prelude

import Data.BigInt (BigInt(..), fromInt, fromString, pow, toNumber, toString,
                    abs)
import Data.Int (fromNumber)
import Data.Foldable (any, foldl, foldr)
import Data.List (List(..), drop, elemIndex, filter, fromFoldable, index,
                  length, reverse, slice, snoc, toUnfoldable, (..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ratio (Ratio(..), denominator, numerator)
import Data.String (count, fromCharArray, toCharArray)




two = one + one


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
        in calculatePrimes' two Nil
    | otherwise = Nil


type BasisFunctions =
    {   fromStringFunction  :: Int -> List Char -> Maybe (Ratio BigInt)
    -- ,   toStringFunction    :: Int -> Ratio BigInt -> List Char
    ,   isFinitFunction     :: Int -> Ratio BigInt -> Maybe Boolean
    }



createBasisFunctions :: List Char -> Maybe BasisFunctions
createBasisFunctions digits =
    -- Basis smaller equal one do not make sense
    if basisMax > 1 then Just basisFunctions else Nothing
      where
        basisMax = length digits
        -- The prime factorizations of all possible basis' of a list of digits
        -- is used for several calculations eg. checking, if a fraction has a
        -- init representation in a certain basis. As it is expensive to
        -- calculate primes and prime factorizations their are calculated for
        -- one module

        primeFactorsList :: List (List BigInt)
        primeFactorsList = do
            basis <- 2 .. basisMax
            let factorization = factorize primes basis
            pure $ map fromInt factorization.factors
          where
            primes = calculatePrimes basisMax

        getPrimeFactors basis = primeFactorsList `index` (basis - 2)

        isFinit basis (Ratio _ denominator)
            | basis >= basisMax = Nothing
            -- If the denominator can be complete factorized by the primefactors
            -- of the current basis, the non-fractional rendering of the
            -- rational is finit
            | otherwise         = Just (foldl factorizeMany denominator primeFactors == one)
              where
                primeFactors = fromMaybe Nil (getPrimeFactors basis)

                factorizeMany number factor
                    | number `mod` factor == zero = factorizeMany (number / factor) factor
                    | otherwise                   = number

        -- Match possible negative sign, parse remaining string and negate result
        fromString basis ('-' : string) =
          do
            ratio <- fromString basis string
            pure (-ratio)
        fromString basis string
            | 1 < basis && basis <= basisMax = maybeRatio
              where
                basisbi = fromInt basis

                point = fromInt $ fromMaybe (length string) ('.' `elemIndex` string)
                shift = (fromInt $ length string) - point
                denominator = basisbi `pow` shift

                string' = filter (\c -> c /= '.') string

                parseDigits :: List Char -> BigInt -> BigInt -> Maybe BigInt
                parseDigits Nil            accumulator _        = Just accumulator
                parseDigits (char : chars) accumulator position =
                -- TODO this is not tail recursive...
                  do
                    digitValue <- char `elemIndex` digits
                    let positionValue   = basisbi `pow` position
                    let delta           = (fromInt digitValue) * positionValue

                    parseDigits chars (accumulator + delta) (position + (fromInt 1))

                maybeRatio :: Maybe (Ratio BigInt)
                maybeRatio =
                  do
                    numerator <- parseDigits string' zero zero
                    pure $ Ratio numerator denominator
            | otherwise = Nothing

        toString :: Int -> Ratio BigInt -> Maybe (List Char)
        toString basis ratio@(Ratio numerator denominator)
            | basis >= basisMax = Nothing
            | otherwise = do
                finit <- isFinit basis ratio

                -- Seperate the *propper* part of the fraction and the
                -- *remainder*
                let propper = abs $ numerator / denominator
                let remainder = ratio - (Ratio propper one)

                -- Calculate *pre* and *post* radix strings
                pre <- stringFromBase Nil propper
                post <- getPost (pseudoFloatFromRatio remainder) basis finit
                let string = pre <> (Cons '.' Nil) <> post

                -- TODO Alter string for display

                pure $ string
              where
                basisbi = fromInt basis
                -- Calculate a string representation of `dividend` in `basis`
                stringFromBase :: List Char -- Accumulator for output string
                               -> BigInt    -- Accumulator for number
                               -> Maybe (List Char)
                stringFromBase cs dividend
                    | dividend >= one = do
                        -- Calculate quotient and remainder of division by
                        -- basis
                        let remainder = dividend `mod` basisbi
                        let quotient = (dividend - remainder) / basisbi
                        -- Get Corresponding digit character
                        i <- fromNumber <<< toNumber $ remainder
                        c <- digits `index` i

                        stringFromBase (c : cs) quotient
                    | otherwise = Just $ cs

                getPost _ _ _ = Just (Cons 'c' Nil)

        basisFunctions =    {   fromStringFunction  : fromString
                            -- ,   toStringFunction    : toString
                            ,   isFinitFunction     : isFinit
                            }


ten = fromInt 10 :: BigInt


-- TODO more usefull default
bigIntFromCharList :: List Char -> BigInt
bigIntFromCharList = (fromMaybe zero) <<< fromString <<< fromCharArray <<< toUnfoldable

charListFromBigInt :: BigInt -> List Char
charListFromBigInt = fromFoldable <<< toCharArray <<< toString


data PseudoFloat = PseudoFloat
    {   finit   :: BigInt
    ,   infinit :: BigInt
    ,   shift   :: Int
    }

-- TODO use generics, problem: bigdata cannot derive generics
instance showPseudoFloat :: Show PseudoFloat where
    show (PseudoFloat dr) =
        "{finit : " <> (show dr.finit)
        <> ", infinit : " <> (show dr.infinit)
        <> ", shift : " <> (show dr.shift) <> "}"

pseudoFloatFromRatio :: Ratio BigInt -> PseudoFloat
pseudoFloatFromRatio (Ratio numerator denominator) = divide numerator Nil Nil zero
  where
    divide :: BigInt       -- Current divident
           -> List BigInt  -- List of previous dividents
           -> List Char    -- List of whole quotients
           -> Int          -- Counter
           -> PseudoFloat
    divide dividend previousDividends quotients counter
        | dividend == zero = PseudoFloat
            {   finit   : bigIntFromCharList quotients
            ,   infinit : zero
            ,   shift   : counter
            }
        | otherwise =
            case dividend `elemIndex` previousDividends of
                -- In case of recurrence, return the result, otherwise, divide
                -- the remaining numerator further
                Just i_infinit ->
                    let infinit = bigIntFromCharList (drop i_infinit quotients)
                        finit = bigIntFromCharList quotients - infinit
                    in  PseudoFloat
                        {   finit   : finit
                        ,   infinit : infinit
                        ,   shift   : counter
                        }
                Nothing ->
                    divide dividend' previousDividends' quotients' counter'
                      where
                        counter' = counter + one
                        previousDividends' = dividend : previousDividends
                        -- Factorize by the current denominator, and save the
                        -- factor to the string of quotients
                        factor = charListFromBigInt $ dividend / denominator
                        quotients' = foldr (:) quotients (reverse factor)
                        dividend' = (dividend `mod` denominator) * ten
