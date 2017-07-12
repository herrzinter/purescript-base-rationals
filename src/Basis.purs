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
import Data.List (List(..), findIndex, take, drop, elemIndex, filter, fromFoldable, index, length, reverse, slice, init, tail, last, head, snoc, toUnfoldable, (..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ratio (Ratio(..), denominator, numerator)
import Data.String as String
import Data.Either (Either (..))
import Control.Error.Util (note)

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
                post <- getPost' basis finit (pseudoFloatFromRatio remainder)
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


ten = fromInt 10 :: BigInt


-- TODO more usefull default
bigIntFromCharList :: List Char -> BigInt
bigIntFromCharList = (fromMaybe zero) <<< fromString <<< String.fromCharArray <<< toUnfoldable

charListFromBigInt :: BigInt -> List Char
charListFromBigInt = fromFoldable <<< String.toCharArray <<< toString


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
pseudoFloatFromRatio (Ratio numerator denominator) = divide numerator Nil Nil (-one)
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
                    let i_drop  = length quotients - i_infinit - one
                        infinit = bigIntFromCharList $ drop i_drop quotients
                        finit   = bigIntFromCharList quotients - infinit
                    in  PseudoFloat {finit, infinit, shift : counter}
                Nothing ->
                    divide dividend' previousDividends' quotients' counter'
                      where
                        counter' = counter + one
                        previousDividends' = dividend : previousDividends
                        -- Factorize by the current denominator, and save the
                        -- factor to the string of quotients
                        factor = charListFromBigInt $ dividend / denominator
                        quotients' = quotients <> factor
                        dividend' = (dividend `mod` denominator) * ten


getPost
    :: List Char          -- Digits
    -> Int                -- Base
    -> Boolean            -- Is the number reccurrent in the base
    -> PseudoFloat        -- Remainder as pseudo float
    -> Either String (List Char)  -- Post radix string
getPost digits basis isFinit pf@(PseudoFloat f0) = loop zero Nil Nil pf
  where
    basisBI = fromInt basis
    shift = ten `pow` (fromInt f0.shift)

    loop
        :: BigInt             -- Counter
        -> List BigInt        -- Intermediate values to check for reccurence
        -> List Char          -- Accumulator for the output string
        -> PseudoFloat        -- Intermediate value
        -> Either String (List Char)  -- Output String
    loop j fs cs (PseudoFloat float)
        -- If the finit part of the pseudo float is zero, then everything has
        -- been expressed in the output string -> Return
        | float.finit == zero  = Right cs
        -- Otherwise, try to express yet more of the intermediate value in
        -- a character in the output base
        | otherwise = case float.finit `elemIndex` fs of
            -- Recurrence -> return with parantheses marking recurrence
             Just i -> Right $
                take i cs <> (Cons '[' Nil) <> drop i cs <> (Cons ']' Nil)
            -- No recurrence -> calculate next step
             Nothing -> do
                -- Update float based on calculations with the infinit part
                let (PseudoFloat float') = ((PseudoFloat float) `scalePseudoFloat` basisBI)

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

                loop (j + one) (float.finit : fs) (c : cs) (PseudoFloat float0'')

scalePseudoFloat
    :: PseudoFloat  -- Input
    -> BigInt       -- Scaling factor
    -> PseudoFloat  -- Output
scalePseudoFloat pseudoFloat@(PseudoFloat pseudoFloatRec) factor
    | not $ isRecurring pseudoFloat = PseudoFloat
        pseudoFloatRec {finit = pseudoFloatRec.finit * factor}
    | otherwise = PseudoFloat
        pseudoFloatRec   {   finit   = pseudoFloatRec.finit * factor + carry
                         ,   infinit = infinit'' - carry * infinitShift
                         }
  where
    -- Calculate the new infinit part, and the carry for the finit part
    -- by multiplying (possibly multiple times) the infinit part with
    -- the factor.
    -- First, the finit and infinit part are multiplied by the factor. If
    -- the number is infinetly recurring, the carry from the infinet
    -- recurrence are calculated. In order to do so, it is estimated, how
    -- many digits the carry is long and thus, how many recurrences
    -- influence the *last* infinit part, and the finit part.
    -- Eg.: If the recurrence is '91' and it is multiplied by factor '10000',
    -- then the new value is '9100000' which is 5 digits longer then the
    -- recurrence. Thus, one has to take into account the carries from 3
    -- previous infinit parts. This is done in the *loop* function.

    numberOfInfinitDigits = countDigits pseudoFloatRec.infinit
    infinit' = pseudoFloatRec.infinit * factor
    numberOfInfinitDigits' = countDigits infinit'

    infinit'' = loop numberOfInfinitDigits infinit'
    infinitShift = ten `pow` numberOfInfinitDigits
    carry = infinit'' / infinitShift

    loop i infinit
        | i < numberOfInfinitDigits' =
            loop
                (i + numberOfInfinitDigits)
                (infinit + pseudoFloatRec.infinit / (ten `pow` i))
        | otherwise = infinit

isRecurring :: PseudoFloat -> Boolean
isRecurring (PseudoFloat pseudoFloatRec) = pseudoFloatRec.infinit /= zero

countDigits :: BigInt -> BigInt
countDigits = fromInt <<< String.length <<< toString
