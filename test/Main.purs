module Test.Main where

import Basis
import Data.Maybe
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.MonadZero (guard)
import Data.Array (concat, foldl)
import Data.BigInt (BigInt(..), fromInt, fromString, toString)
import Data.Either (Either(..))
import Data.Function (($))
import Data.List (fromFoldable)
import Data.Ratio (Ratio(..), numerator, denominator)
import Control.Error.Util (note)



data Quartet = QuartetInt     Int String Int    Int
             | QuartetString  Int String String String

quartets =  [   (QuartetInt 10 "0" 0 1)

            ,   (QuartetInt 10 "100" 100 1)
            ,   (QuartetInt 10 "10" 10 1)
            ,   (QuartetInt 10 "1" 1 1)
            ,   (QuartetInt 10 "0.1" 1 10)
            ,   (QuartetInt 10 "0.01" 1 100)

            ,   (QuartetInt 10 "54389" 54389 1)
            ,   (QuartetInt 10 "234.1" 2341 10)
            ,   (QuartetInt 10 "897.01389" 89701389 100000)

            ,   (QuartetInt  2 "1" 1 1)
            ,   (QuartetInt  2 "0.1" 1 2)
            ,   (QuartetInt  2 "0.01" 1 4)
            ,   (QuartetInt  2 "10" 2 1)
            ,   (QuartetInt  2 "100" 4 1)
            ,   (QuartetInt  2 "1101" 13 1)
            ]

quartets' = [   (QuartetInt 10 "0.[3]"  1 3)
            ,   (QuartetInt 10 "0.[076923]" 1 13)

            ,   (QuartetInt  2 "1011.00[01]" 133 12)

            ,   (QuartetInt 12 "2.3[7249]" 23 10)

            ,   (QuartetInt 16 "B2.D[6DB]" 20030 112)
            ,   (QuartetInt 16 "F01.A" 61466 16)
            ,   (QuartetString 10 "120391239810293098098098098098888888888888888888888888888888888888888888888888888880909809809809809809809809808120293842093840932843120391239810293098098098098098888888888888888888888888888888888888888888888888888880909809809809809809809809808120293842093840932843120391239810293098098098098098888888888888888888888888888888888888888888888888888880909809809809809809809809808120293842093" "120391239810293098098098098098888888888888888888888888888888888888888888888888888880909809809809809809809809808120293842093840932843120391239810293098098098098098888888888888888888888888888888888888888888888888888880909809809809809809809809808120293842093840932843120391239810293098098098098098888888888888888888888888888888888888888888888888888880909809809809809809809809808120293842093" "1" )
            ,   (QuartetInt 10 "0.[005623072737166696898240522401596227099582804280790857972066025757300925086159985488844549247233810992200253945220388173408307636495555958643206965354616361327770723743878106294213676764012334482133139851260656629784146562670052602938508978777435153274079448576092871394884817703609649918374750589515690186831126428441864683475421730455287502267368039180119717032468710321059314347904951931797569381461998911663341193542535824415019045891529113]"  31 5513)
            ,   (QuartetInt 16 "20A20.[B31F84FF3]" 44510345 333)
            ]

data QuartetBigInt =  QuartetBigInt Int String BigInt BigInt

quartets2bigintQuartets :: Quartet -> Either String QuartetBigInt
quartets2bigintQuartets (QuartetInt basis s ni di) = do
    pure $ QuartetBigInt basis s (fromInt ni) (fromInt di)
quartets2bigintQuartets (QuartetString basis s ns ds) = do
    nbi <- note "Conversion failed" (fromString ns)
    dbi <- note "Conversion failed" (fromString ds)
    pure $ QuartetBigInt basis s nbi dbi

digits :: Array Char
digits = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']


testToString
    :: (Int -> (Ratio BigInt) -> Either String String)
    -> Array String
testToString toString' = do
    quartet <- quartets <> quartets'

    case quartets2bigintQuartets quartet of
        Left s -> pure s
        Right (QuartetBigInt basis s (n :: BigInt) d) -> do
            let operation = "(" <> toString n <> " % " <> toString d <> ") in basis " <>
                show basis <> "-> " <> s

            case toString' basis (Ratio n d) of
                Left error ->
                    pure $ "Operation failed: " <> operation
                        <> " with '" <> error <> "'"
                Right string -> do
                    guard $ string /= s

                    pure $ "Operation failed: " <> operation <>
                        " with result " <> string

testFromString
    :: (Int -> String -> Either String (Ratio BigInt))
    -> Array String
testFromString fromString = do
    quartet <- quartets

    case quartets2bigintQuartets quartet of
        Left s -> pure "Could not read operation"
        Right (QuartetBigInt basis s n d) -> do
            let operation = s <> " in basis " <> show basis <>
                "-> " <> " /= (" <> toString n <> " % " <> toString d <> ")"

            case fromString basis s of
                Left error ->
                    pure $ "Operation failed: " <> operation
                        <> " with '" <> error <> "'"
                Right fraction -> do
                    guard $ numerator fraction /= n || denominator fraction /= d

                    pure $ "Operation failed: " <> operation <> " with result ("
                        <> (toString $ numerator fraction) <> " % "
                        <> (toString $ denominator fraction) <> ")"


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  case createBasisFunctions digits of
      Just {fromString, toString, isFinit} -> do
          let tests = testToString toString <> testFromString fromString
          log $ foldl (\a s -> a <> "\n" <> s) "" tests
      Nothing -> log "Could not create basis functions"
