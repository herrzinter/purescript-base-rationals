module Basis where


import Prelude
import Control.Bind ((=<<))
import Data.Array as Array
import Data.BigInt (BigInt(..), fromInt, fromString, pow, toNumber, toString, abs)
import Data.Either (Either)
import Data.EuclideanRing (class EuclideanRing)
import Data.Foldable (any, foldl, foldr)
import Data.Function ((#))
import Data.Int (fromNumber)
import Data.List (List(..), findIndex, take, drop, elemIndex, dropWhile, filter, fromFoldable, index, length, reverse, slice, init, tail, last, head, snoc, toUnfoldable, (..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ratio (Ratio(..), denominator, numerator)
import Data.String as String
import Data.Either (Either (..))
import Control.Error.Util (note)
import Control.Monad.Rec.Class (Step (..), tailRec, tailRecM, tailRecM2, class MonadRec)

import Precise


two = one + one


tailRecM4
  :: forall m a b c d e
   . MonadRec m
  => (a -> b -> c -> d -> m (Step { a :: a, b :: b, c :: c, d :: d } e))
  -> a
  -> b
  -> c
  -> d
  -> m e
tailRecM4 f a b c d = tailRecM (\o -> f o.a o.b o.c o.d) { a, b, c, d}



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
    {   isFinit     :: Int -> Ratio BigInt -> Maybe Boolean
    ,   fromString  :: Int -> String       -> Either String (Ratio BigInt)
    ,   toString    :: Int -> Ratio BigInt -> Either String String
    }



createBasisFunctions :: Array Char -> Maybe BasisFunctions
createBasisFunctions digitsArray =
    -- Basis smaller equal one do not make sense
    if basisMax > 1 then Just basisFunctions else Nothing
      where
        digits = fromFoldable digitsArray
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
            | basis > basisMax = Nothing
            -- If the denominator can be complete factorized by the primefactors
            -- of the current basis, the non-fractional rendering of the
            -- rational is finit
            | otherwise         = Just (foldl factorizeMany denominator primeFactors == one)
              where
                primeFactors = fromMaybe Nil (getPrimeFactors basis)

                factorizeMany number factor
                    | number `mod` factor == zero = factorizeMany (number / factor) factor
                    | otherwise                   = number

        fromString' :: Int -> String -> Either String (Ratio BigInt)
        fromString' basis =
            fromString basis <<< fromFoldable <<< String.toCharArray

        -- Match possible negative sign, parse remaining string and negate result
        fromString :: Int -> List Char -> Either String (Ratio BigInt)
        fromString basis ('-' : string) =
          do
            ratio <- fromString basis string
            pure (-ratio)
        fromString basis string
            | 1 < basis && basis <= basisMax = maybeRatio
              where
                basisbi = fromInt basis

                point = fromMaybe (length string) ('.' `elemIndex` string)
                string' = filter (\c -> c /= '.') string
                shift = fromInt $ length string' - point
                denominator = basisbi `pow` shift

                parseDigits :: List Char -> BigInt -> BigInt -> Either String BigInt
                parseDigits Nil            accumulator _        = Right accumulator
                parseDigits (char : chars) accumulator position =
                -- TODO this is not tail recursive...
                  do
                    digitValue <- note
                        "Failed to lookup character"
                        (char `elemIndex` digits)
                    let positionValue   = basisbi `pow` position
                    let delta           = (fromInt digitValue) * positionValue

                    parseDigits chars (accumulator + delta) (position + one)

                maybeRatio :: Either String (Ratio BigInt)
                maybeRatio =
                  do
                    numerator <- parseDigits (reverse string') zero zero
                    pure $ Ratio numerator denominator
            | otherwise = Left "Basis outside valid range"

        getPost' = getPost digits

        toString :: Int -> Ratio BigInt ->  Either String String
        toString basis ratio@(Ratio numerator denominator)
            | basis > basisMax = Left $ "Basis exceeds " <> show basis
            | otherwise = do
                finit <- note
                    "Failed to check if number is finit in output base"
                    (isFinit basis ratio)

                -- Seperate the *propper* part of the fraction and the
                -- *remainder*
                let propper = abs $ numerator / denominator
                let remainder = ratio - (Ratio propper one)

                -- Calculate *pre* and *post* radix strings
                pre <- stringFromBase Nil propper
                post <- getPost' basis finit (fromRatio remainder)
                let string = pre <> (Cons '.' Nil) <> post

                -- TODO Alter string for display
                string' <- note "String is empty" (cleanString string)


                pure <<< String.fromCharArray <<< toUnfoldable $ string'
              where
                basisbi = fromInt basis
                -- Calculate a string representation of `dividend` in `basis`
                stringFromBase :: List Char -- Accumulator for output string
                               -> BigInt    -- Accumulator for number
                               -> Either String (List Char)
                stringFromBase cs dividend
                    | dividend >= one = do
                        -- Calculate quotient and remainder of division by
                        -- basis
                        let remainder = dividend `mod` basisbi
                        let quotient = (dividend - remainder) / basisbi
                        -- Get Corresponding digit character
                        i <- note
                            "Failed to convert numbers"
                            (fromNumber <<< toNumber $ remainder)
                        c <- note
                            "Failed to lookup character"
                            (digits `index` i)

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

        basisFunctions =    {   fromString  : fromString'
                            ,   toString    : toString
                            ,   isFinit     : isFinit
                            }


getPost
    :: List Char          -- Digits
    -> Int                -- Base
    -> Boolean            -- Is the number reccurrent in the base
    -> PseudoFloat        -- Remainder as pseudo float
    -> Either String (List Char)  -- Post radix string
getPost digits basis isFinit pf@(PseudoFloat f0) = tailRecM4 loop zero Nil Nil pf
  where
    basisBI = fromInt basis
    shift = ten `pow` (fromInt f0.shift)

    loop
        :: BigInt             -- Counter
        -> List BigInt        -- Intermediate values to check for reccurence
        -> List Char          -- Accumulator for the output string
        -> PseudoFloat        -- Intermediate value
        -> Either String (Step {a::BigInt, b::(List BigInt), c::(List Char), d::(PseudoFloat)} (List Char))  -- Output String
    loop j fs cs (PseudoFloat float)
        -- If the finit part of the pseudo float is zero, then everything has
        -- been expressed in the output string -> Return
        | float.finit == zero  = Right $ Done cs
        -- Otherwise, try to express yet more of the intermediate value in
        -- a character in the output base
        | otherwise = case float.finit `elemIndex` fs of
            -- Recurrence -> return with parantheses marking recurrence
             Just i ->
              let
                i_drop  = length fs - i - one
              in
                Right $ Done $ take i_drop cs <> (Cons '[' Nil) <> drop i_drop cs <> (Cons ']' Nil)
            -- No recurrence -> calculate next step
             Nothing -> do
                -- Update float based on calculations with the infinit part
                (PseudoFloat float') <- (PseudoFloat float) `scale` basisBI

                let carry = if j == zero && isFinit == true then one else zero
                let float'' = float' {finit = float'.finit + carry}

                -- Calculate index *i* and corresponding char *c*
                let iBI = float''.finit / shift
                i <- note
                    "Failed to convert numbers"
                     (fromNumber $ toNumber iBI)
                c <- note
                    "Failed to lookup character"
                    (digits `index` i)

                -- Update finit part according to index *i*
                let float0'' = float'' {finit = float''.finit - iBI * shift}

                pure $ Loop $ {a: (j + one), b: (float.finit : fs), c: (c : cs), d: (PseudoFloat float0'')}
