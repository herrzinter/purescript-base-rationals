module Test.Main where

import Basis
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.BigInt (toString)
import Data.Function (($))
import Data.List (fromFoldable)
import Data.Maybe
import Data.BigInt (BigInt (..), fromInt)
import Data.Ratio (Ratio (..), numerator, denominator)
import Control.MonadZero (guard)

digits :: Array Char
digits = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']

data Quartet = Quartet Int String Int Int

fromStringTests =   [   (Quartet 10 "0" 0 1)

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

                    ,   (Quartet 16 "F01.A" 61466 16)
                    ]

array2tests array fromString = do
    (Quartet basis s n d) <- array

    case fromString basis s of
        Nothing -> pure ""
        Just fraction -> do
            guard $ numerator fraction /= fromInt n || denominator fraction /= fromInt d

            pure $ "Result (" <> (toString $ numerator fraction)
                <> " % " <> (toString $ denominator fraction) <> ")"
                <> " parsed " <> s
                <> " /= (" <> show n <> " % " <> show d <> ")"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  case createBasisFunctions digits of
      Just {fromString, toString, isFinit} ->
          log <<< show $ array2tests fromStringTests fromString

      Nothing -> log "Could not create basis functions"
