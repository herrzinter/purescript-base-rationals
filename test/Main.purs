module Test.Main where

import Basis
import Data.Maybe
import Prelude

import Control.Bind ((=<<))
import Control.Error.Util (note)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.MonadZero (guard)
import Data.Array (concat, foldl, (..))
import Data.BigInt (BigInt(..), fromInt, fromString, toString)
import Data.Either (Either(..))
import Data.Function (($))
import Data.List (fromFoldable)
import Data.Ratio (Ratio(..), numerator, denominator)

import PreciseFloat (PreciseFloat (..), scale, fromRatio)


pfFromRatioTestArray =
    [   PFFromRatioTestInt     1 2                5 0 0 1
    ,   PFFromRatioTestInt     1 3                0 3 1 1
    ,   PFFromRatioTestInt     89701389 100000    89701389 0 0 5
    ,   PFFromRatioTestInt     1 100              1 0 0 2
    ,   PFFromRatioTestInt     1230 10            123 0 0 0
    ,   PFFromRatioTestInt     5 11               0 45 2 2
    ,   PFFromRatioTestInt     10000 10           1000 0 0 0
    ,   PFFromRatioTestString "123189" "35"       "35196000000" "857142" 6 7
    ,   PFFromRatioTestString "89701389" "35"     "25628968000000" "285714" 6 7
    ,   PFFromRatioTestInt     10 13              0 769230 6 6
    ]

data PreciseFloatTest
    = PFFromRatioTestString String String String String Int Int
    | PFFromRatioTestInt    Int    Int    Int    Int    Int Int


readPFFromRatioTest (PFFromRatioTestString ns ds fs is ils s) = do
    n <- fromString ns
    d <- fromString ds
    finit <- fromString fs
    infinit <- fromString is

    let pf' = PreciseFloat {finit, infinit, infinitLength : ils, shift : s}
    let r   = Ratio n d

    pure {r, pf'}
readPFFromRatioTest (PFFromRatioTestInt n d f i infinitLength shift) = do
    let finit   = fromInt f
    let infinit = fromInt i

    let pf' = PreciseFloat {finit, infinit, infinitLength, shift}
    let r   = Ratio (fromInt n) (fromInt d)

    pure {r, pf'}

testPfFromRatio = do
    pfFromRatioTest <- pfFromRatioTestArray

    case readPFFromRatioTest pfFromRatioTest of
        Nothing           -> pure "Failed to read creating PreciseFloatTest"
        Just {r, pf'}  -> do
            let pf = fromRatio r
            guard $ pf /= pf'
            pure $ "Creating PreciseFloat failed with " <> show pf <> show pf'


pfScalingTestArray =
    [   PfScalingTestInt 0 0 0 0        0 0 0 0            30493
    ,   PfScalingTestInt 7891 0 0 0     72510399 0 0 0     9189
    ,   PfScalingTestInt 120 1 1 1      109 0 0 0          9
    ,   PfScalingTestInt 1030 2 1 2     3090 6 1 2         3
    ,   PfScalingTestInt 0 1 1 5        0 1 1 3            100
    ,   PfScalingTestInt 0 12 2 2       0 36 2 2           3
    ,   PfScalingTestInt 0 45 2 2       100 36 2 2         3
    ,   PfScalingTestInt 57000 819 3 7  520436000 198 3 7  9001
    ,   PfScalingTestInt 0 3 1 1        30 3 1 1           10
    ,   PfScalingTestInt 1 0 0 1        1 0 0 0            10
    ,   PfScalingTestInt 0 76923 6 6    0 769230 6 6       10
    ]

data PfScalingTest
    = PfScalingTestInt Int Int Int Int Int Int Int Int Int

readPfScalingTest (PfScalingTestInt f1 i1 il1 s1   f2 i2 il2 s2 factor) =
  let
    pf1 = PreciseFloat
        {   finit         : fromInt f1
        ,   infinit       : fromInt i1
        ,   infinitLength : il1
        ,   shift         : s1
        }
    pf2 = PreciseFloat
        {   finit         : fromInt f2
        ,   infinit       : fromInt i2
        ,   infinitLength : il2
        ,   shift         : s2
        }

    in
      {pf1, pf2, factor : fromInt factor}


testPfScaling = do
    pfScalingTest <- pfScalingTestArray

    let {pf1, pf2, factor} = readPfScalingTest pfScalingTest

    case pf1 `scale` factor of
        Left e                    -> pure e
        Right pf1' -> do

            guard $ pf2 /= pf1'

            pure $ "Scaling PreciseFloat failed with "
                <> show pf2 <> " " <> show pf1'


digits :: Array Char
digits = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']

data TestToFromString = TestToFromStringString Int String String String
                      | TestToFromStringInt    Int String Int    Int

toFromStringTestArray =
    [   (TestToFromStringInt 10 "0" 0 1)

    ,   (TestToFromStringInt 10 "100" 100 1)
    ,   (TestToFromStringInt 10 "10" 10 1)
    ,   (TestToFromStringInt 10 "1" 1 1)
    ,   (TestToFromStringInt 10 "0.1" 1 10)
    ,   (TestToFromStringInt 10 "0.01" 1 100)

    ,   (TestToFromStringInt 10 "54389" 54389 1)
    ,   (TestToFromStringInt 10 "234.1" 2341 10)
    ,   (TestToFromStringInt 10 "897.01389" 89701389 100000)

    ,   (TestToFromStringInt  2 "1" 1 1)
    ,   (TestToFromStringInt  2 "0.1" 1 2)
    ,   (TestToFromStringInt  2 "0.01" 1 4)
    ,   (TestToFromStringInt  2 "10" 2 1)
    ,   (TestToFromStringInt  2 "100" 4 1)
    ,   (TestToFromStringInt  2 "1101" 13 1)

    ,   (TestToFromStringInt  3 "1.1" 4 3)
    ]

toFromStringTestArray' =
    [   (TestToFromStringInt 10 "0.[3]"  1 3)
    ,   (TestToFromStringInt 10 "0.[076923]" 1 13)

    ,   (TestToFromStringInt  2 "1011.00[01]" 133 12)

    ,   (TestToFromStringInt 12 "2.3[7249]" 23 10)

    ,   (TestToFromStringInt 16 "B2.D[6DB]" 20030 112)
    ,   (TestToFromStringInt 16 "F01.A" 61466 16)
    ,   (TestToFromStringString 10 "120391239810293098098098098098888888888888888888888888888888888888888888888888888880909809809809809809809809808120293842093840932843120391239810293098098098098098888888888888888888888888888888888888888888888888888880909809809809809809809809808120293842093840932843120391239810293098098098098098888888888888888888888888888888888888888888888888888880909809809809809809809809808120293842093" "120391239810293098098098098098888888888888888888888888888888888888888888888888888880909809809809809809809809808120293842093840932843120391239810293098098098098098888888888888888888888888888888888888888888888888888880909809809809809809809809808120293842093840932843120391239810293098098098098098888888888888888888888888888888888888888888888888888880909809809809809809809809808120293842093" "1" )
    ,   (TestToFromStringInt 10 "0.[005623072737166696898240522401596227099582804280790857972066025757300925086159985488844549247233810992200253945220388173408307636495555958643206965354616361327770723743878106294213676764012334482133139851260656629784146562670052602938508978777435153274079448576092871394884817703609649918374750589515690186831126428441864683475421730455287502267368039180119717032468710321059314347904951931797569381461998911663341193542535824415019045891529113]"  31 5513)
    ,   (TestToFromStringInt 16 "20A20.[B31F84FF3]" 44510345 333)
    ]

readToFromTest (TestToFromStringInt b s ni di) = do
    let r = Ratio (fromInt ni) (fromInt di)

    pure {b, s, r}
readToFromTest (TestToFromStringString b s ns ds) = do
    nbi <- note "Conversion failed" (fromString ns)
    dbi <- note "Conversion failed" (fromString ds)

    let r = Ratio nbi dbi

    pure {b, s, r}

testToString
    :: (Int -> (Ratio BigInt) -> Either String String)
    -> Array String
testToString toString' = do
    toFromStringTest <- toFromStringTestArray <> toFromStringTestArray'

    case readToFromTest toFromStringTest of
        Left s              -> pure s
        Right {b, s, r} -> do
            let operation = "toString " <> show r <> " b: " <> show b <> " -> "
                         <> "'" <> s <> "'"

            case toString' b r of
                Left error ->
                    pure $ operation <> " failed with error '" <> error <> "'"
                Right string -> do
                    guard $ string /= s
                    pure $ operation <> " failed with result " <> string

testFromString
    :: (Int -> String -> Either String (Ratio BigInt))
    -> Array String
testFromString fromString' = do
    toFromStringTest <- toFromStringTestArray

    case readToFromTest toFromStringTest of
        Left s              -> pure s
        Right {b, s, r: r@(Ratio rn rd)} -> do
            let operation = "fromString '" <> s <> "' b: " <> show b <> " -> "
                         <> show r

            case fromString' b s of
                Left error ->
                    pure $ operation <> " failed with error '" <> error <> "'"
                Right r'@(Ratio rn' rd') -> do
                    guard $ rn /= rn' || rd /= rd'
                    pure $ operation <> " failed with result " <> show r'


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  case createBasisFunctions digits of
      Just {fromString, toString, isFinit} -> do
          let tests = testPfScaling
                    <> testPfFromRatio
                    <> testToString toString
                    <> testFromString fromString
          log $ foldl (\a s -> a <> "\n" <> s) "" tests
      Nothing -> log "Could not create basis functions"
