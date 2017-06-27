module Basis where


import Prelude
import Data.List (List (..), length, index, elemIndex, (..), (:), filter,
                  reverse, fromFoldable, snoc)
import Data.String (toCharArray)
import Data.Foldable (any, foldl, foldr)
import Data.Ratio (Ratio (..))
import Data.Maybe (Maybe (..), fromMaybe)
import Data.BigInt (pow, BigInt (..), fromInt, toString, toNumber)




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
        in factorizeRecursive factors {fs : Nil, remainder : number}
    | otherwise = {fs : Nil, remainder : zero}


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


type BasisFunctions =   {   getDigits           :: Maybe Int -> List Char
                        ,   fromStringFunction  :: Int -> List Char -> Maybe (Ratio BigInt)
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
            | 1 < basis || basis <= basisMax = maybeRatio
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

        -- toString :: Int -> Ratio BigInt -> List Char
        -- toString basis (Ratio numerator denominator) | basis >= basisMax = Nil
              -- where
                -- TODO This can not fail, nevertheless it should be handled properly
                -- finit = fromMaybe false (isFinit basis ratio)

                -- test = {
                --     finit           : fromString pseudoFloat.string
                --     infinit         : do
                --         infinit <- pseudoFloat.infinit

                --     infinitlength   : length pseudoFloat.string - pseudoFloat.infinit
                --     shift           :
                -- }

        basisFunctions =    {   getDigits           : \_ -> digits
                            ,   fromStringFunction  : fromString
                            -- ,   toStringFunction    : toString
                            ,   isFinitFunction     : isFinit
                            }


ten = fromInt 10 :: BigInt

data DivisionResult = DivisionResult    {   finit   :: BigInt
                                        ,   infinit :: Maybe Int
                                        ,   string  :: List Char
                                        ,   count   :: BigInt
                                        ,   test    :: List BigInt
                                        }

-- divisionResult finit infinit string count test =
--     DivisionResult  {   finit   : finit
--                     ,   infinit : infinit
--                     ,   string  : string
--                     ,   count   : count
--                     ,   test    : test
--                     }

-- TODO use generics, problem: bigdata cannot derive generics
instance showDivisionResult :: Show DivisionResult where
    show (DivisionResult dr) =
        "{finit : " <> (show dr.finit) <> ", infinit : "
        <> (show dr.infinit) <> ", string : " <> (show dr.string)
        <> ", count : " <> (show dr.count) <> ", test : "
        <> (show dr.test) <> "}"



divide :: Ratio BigInt -> DivisionResult
divide (Ratio numerator denominator) = divide' numerator Nil Nil zero
  where
    divide' :: BigInt -> List BigInt -> List Char -> BigInt -> DivisionResult
    divide' num nums string count
        | num == zero = divisionResult num Nothing string count nums
        | otherwise   =
            case num `elemIndex` nums of
                -- In case of recurrence, return the result, otherwise, divide
                -- the remaining numerator further
                Just infinit    -> divisionResult num (Just infinit) string count nums
                Nothing         ->
                    divide' num' (num : nums) string' (count + one)
                      where
                        -- Factorize by the current denominator, and save the
                        -- factor to the string of factors
                        -- TODO type simplification? At the moment it is
                        -- BigInt -> String -> Array Char -> List Char
                        factor  = fromFoldable $ toCharArray $ toString $ num / denominator
                        string' = foldr (:) string (reverse factor)
                        num'    = (num `mod` denominator) * ten

-- type PseudoFloat = PseudoFloat
--     {   finit           :: BigInt
--     ,   infinit         :: Maybe BigInt
--     ,   infinitLength   :: Int
--     ,   shift           :: BigInt
--     }
-- data Point = Point { x :: Number, y :: Number }
-- createPseudoFloat :: Ratio BigInt -> PseudoFloat
-- createPseudoFloat (Ratio numerator denominator) = pseudoFloat
--   where

--     divisionResult = divideNumerator numerator Nil "" zero

--     infinitLength = (fromInt $ length numeratorDivision.string) - numeratorDivision.infinit
--     finit = fromString numeratorDivision.string

    -- (Tuple finit' infinit') = case numeratorDivision.infinit of
    --     Nothing -> Tuple finit Nothing
    --     Just x  -> Tuple finit infinit
    --       where
    --         infinit = fromString $ drop (toInt infinit) string :: BigInt
    --         finit' = finit - infinit
--     pseudoFloat = PseudoFloat   {   finit           : numeratorDivision.finit
--                                 ,   infinit         :
--                                 ,   infinitlength   :
--                                 ,   shift           : ten `pow` numeratorDivision.count
--                                 }
