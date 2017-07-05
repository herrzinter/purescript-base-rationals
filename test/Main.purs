module Test.Main where

import Basis
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.BigInt (toString)
import Data.Function (($))
import Data.List (fromFoldable)
import Data.Array (concat)
import Data.Maybe
import Data.BigInt (BigInt (..), fromInt)
import Data.Ratio (Ratio (..), numerator, denominator)
import Control.MonadZero (guard)
import Data.Either (Either (..))

digits :: Array Char
digits = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']

data Quartet = Quartet Int String Int Int

quartets =  [   (Quartet 10 "0" 0 1)

            ,   (Quartet 10 "100" 100 1)
            ,   (Quartet 10 "10" 10 1)
            ,   (Quartet 10 "1" 1 1)
            ,   (Quartet 10 "0.1" 1 10)
            ,   (Quartet 10 "0.01" 1 100)

            ,   (Quartet 10 "54389" 54389 1)
            ,   (Quartet 10 "234.1" 2341 10)
            ,   (Quartet 10 "897.01389" 89701389 100000)

            ,   (Quartet  2 "1" 1 1)
            ,   (Quartet  2 "0.1" 1 2)
            ,   (Quartet  2 "0.01" 1 4)
            ,   (Quartet  2 "10" 2 1)
            ,   (Quartet  2 "100" 4 1)
            ,   (Quartet  2 "1101" 13 1)
            ]

quartets' = [   (Quartet 10 "0.[3]"  1 3)
            ,   (Quartet 10 "0.[076923]" 1 13)

            ,   (Quartet  2 "1011.00[01]" 133 12)

            ,   (Quartet 12 "2.3[7249]" 23 10)

            ,   (Quartet 16 "B2.D[6DB]" 20030 112)
            ,   (Quartet 16 "F01.A" 61466 16)
            ]

testToString
    :: (Int -> (Ratio BigInt) -> Either String String)
    -> Array String
testToString toString = do
    (Quartet basis s n d) <- quartets <> quartets'
    let operation = "(" <> show n <> " % " <> show d <> ") in basis " <>
        show basis <> "-> " <> s

    case toString basis (Ratio (fromInt n) (fromInt d)) of
        Left error ->
            pure $ "Operation failed: " <> operation
                <> " with '" <> error <> "'"
        Right string -> do
            guard $ string == s

            pure $ "Operation failed: " <> operation <>
                " with result " <> string

testFromString
    :: (Int -> String -> Either String (Ratio BigInt))
    -> Array String
testFromString fromString = do
    (Quartet basis s n d) <- quartets
    let operation = s <> " in basis " <> show basis <>
        "-> " <> " /= (" <> show n <> " % " <> show d <> ")"

    case fromString basis s of
        Left error ->
            pure $ "Operation failed: " <> operation
                <> " with '" <> error <> "'"
        Right fraction -> do
            guard $ numerator fraction /= fromInt n || denominator fraction /= fromInt d

            pure $ "Operation failed: " <> operation <> " with result ("
                <> (toString $ numerator fraction) <> " % "
                <> (toString $ denominator fraction) <> ")"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  case createBasisFunctions digits of
      Just {fromString, toString, isFinit} ->
          log <<< show $ (testToString toString <> testFromString fromString)
      Nothing -> log "Could not create basis functions"
