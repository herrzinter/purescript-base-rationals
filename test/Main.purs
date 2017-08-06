module Test.Main where

import Prelude
import BaseRationals

import Data.BigInt as BI
import PreciseRational as PR
import PreciseFloat as PF

import Data.Ratio (Ratio)
import Data.List (fromFoldable)
import Data.Array (foldl)
import PreciseFloat (PreciseFloat (..), scale, fromRatio)
import Data.BigInt (BigInt(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe (..))
import Control.Error.Util (note)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.MonadZero (guard)

import Test.Unit (suite, test, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert


testPreciseFloatFromRatio = test "Test creating PreciseFloat from Ratio" do
    testFromRatioInt     1 2                5 0 0 1
    testFromRatioInt     1 3                0 3 1 1
    testFromRatioInt     89701389 100000    89701389 0 0 5
    testFromRatioInt     1 100              1 0 0 2
    testFromRatioInt     1230 10            123 0 0 0
    testFromRatioInt     5 11               0 45 2 2
    testFromRatioInt     10000 10           1000 0 0 0
    testFromRatioString "89701389" "35"     "25628968" "285714" "6" "7"
    testFromRatioString "123189" "35"       "35196" "857142" "6" "7"
    testFromRatioInt     10 13              0 769230 6 6

testFromRatioString ns ds fs is ils ss =
    testFromRatio (PR.fromStrings ns ds) (PF.fromStrings fs is ils ss)

testFromRatioInt n d f i il s =
    testFromRatio (Just $ PR.fromInts n d) (Just $ PF.fromInts f i il s)

testFromRatio (Just r) (Just pf') =
    let pf = fromRatio r
        msg = "`fromRatio` failed: " <> show pf <> " should be " <> show pf'
    in  Assert.assert msg $ pf == pf'
testFromRatio _ _ = failure "Could not read test"


testPreciseFloatScaling = test "Test scaling of `PreciseFloat`s" do
    testScalingInt 0 0 0 0        0 0 0 0            30493
    testScalingInt 7891 0 0 0     72510399 0 0 0     9189
    testScalingInt 12 1 1 1       109 0 0 0          9
    testScalingInt 103 2 1 2      309 6 1 2         3
    testScalingInt 0 1 1 5        0 1 1 3            100
    testScalingInt 0 12 2 2       0 36 2 2           3
    testScalingInt 0 45 2 2       1 36 2 2         3
    testScalingInt 57 819 3 7     520436 198 3 7  9001
    testScalingInt 0 3 1 1        3 3 1 1           10
    testScalingInt 1 0 0 1        1 0 0 0            10
    testScalingInt 0 76923 6 6    0 769230 6 6       10

testScalingInt f1 i1 il1 s1 f2 i2 il2 s2 factor =
  let pf1 = PF.fromInts f1 i1 il1 s1
      pf2 = PF.fromInts f2 i2 il2 s2

      pf1' = pf1 `scale` (BI.fromInt factor)
      msg = "`scale` failed: " <> show pf1' <> " should be " <> show pf2
  in  Assert.assert msg $ pf2 == pf1'


conversionSuite (Left error) = test "" $ failure error
conversionSuite (Right digits) =
    suite ("Test conversion with digits " <> show digits) do
        testConversions digits

testConversions digits = test "Test `toString` and `fromString`" do
    testConversionInt digits 10 "0" 0 1

    testConversionInt digits 10 "100" 100 1
    testConversionInt digits 10 "10" 10 1
    testConversionInt digits 10 "1" 1 1
    testConversionInt digits 10 "0.1" 1 10
    testConversionInt digits 10 "0.01" 1 100

    testConversionInt digits 10 "54389" 54389 1
    testConversionInt digits 10 "234.1" 2341 10
    testConversionInt digits 10 "897.01389" 89701389 100000

    testConversionInt digits  2 "1" 1 1
    testConversionInt digits  2 "0.1" 1 2
    testConversionInt digits  2 "0.01" 1 4
    testConversionInt digits  2 "10" 2 1
    testConversionInt digits  2 "100" 4 1
    testConversionInt digits  2 "1101" 13 1

    testConversionInt digits  3 "1.1" 4 3

    testConversionInt digits 10 "0.[3]"  1 3
    testConversionInt digits 10 "0.[076923]" 1 13

    testConversionInt digits  2 "1011.00[01]" 133 12

    testConversionInt digits 12 "2.3[7249]" 23 10

    testConversionInt digits 16 "B2.D[6DB]" 20030 112
    testConversionInt digits 16 "F01.A" 61466 16
    testConversionString
        digits
        10
        (   "120391239810293098098098098098888888888888888888888888888888888888"
        <>  "888888888888888880909809809809809809809809808120293842093840932843"
        <>  "120391239810293098098098098098888888888888888888888888888888888888"
        <>  "888888888888888880909809809809809809809809808120293842093840932843"
        <>  "120391239810293098098098098098888888888888888888888888888888888888"
        <>  "888888888888888880909809809809809809809809808120293842093"
        )
        (   "120391239810293098098098098098888888888888888888888888888888888888"
        <>  "888888888888888880909809809809809809809809808120293842093840932843"
        <>  "120391239810293098098098098098888888888888888888888888888888888888"
        <>  "888888888888888880909809809809809809809809808120293842093840932843"
        <>  "120391239810293098098098098098888888888888888888888888888888888888"
        <>  "888888888888888880909809809809809809809809808120293842093"
        )
        "1"
    testConversionInt
        digits
        10
        (   "0.[005623072737166696898240522401596227099582804280790857972066025"
        <>  "757300925086159985488844549247233810992200253945220388173408307636"
        <>  "495555958643206965354616361327770723743878106294213676764012334482"
        <>  "133139851260656629784146562670052602938508978777435153274079448576"
        <>  "092871394884817703609649918374750589515690186831126428441864683475"
        <>  "421730455287502267368039180119717032468710321059314347904951931797"
        <>  "569381461998911663341193542535824415019045891529113]"
        )
        31
        5513
    testConversionInt digits 16 "20A20.[B31F84FF3]" 44510345 333


testConversionInt digits basis string n d =
    testConversion digits basis string (Just $ PR.fromInts n d)
testConversionString digits basis string ns ds =
    testConversion digits basis string (PR.fromStrings ns ds)

testConversion digits basis string (Just ratio) = do
    testToString digits basis string ratio
    testFromString digits basis string ratio
testConversion _ _ _ _ = failure "Could not read test"

testToString digits basis s ratio =
    let operation = "toString " <> show basis <> " " <> show ratio
    in  case toString digits basis ratio of
            Left error  -> failure $ operation <> " failed: " <> error
            Right s'    -> Assert.assert
                (operation <> " failed: " <> s' <> " should be " <> s)
                (s == s')

testFromString digits basis string r =
    let operation = "fromString " <> show basis <> " " <> show string
    in  case fromString digits basis string of
            Left error  -> failure $ operation <> " failed: " <> error
            Right r'    -> Assert.assert
                (operation <> " failed: " <> show r' <> " should be " <> show r)
                (r == r')


digitArray :: Array Char
digitArray = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']

main = runTest do
    suite "Test `PreciseFloat`s" do
        testPreciseFloatFromRatio
        testPreciseFloatScaling
    conversionSuite (digitsFromArray digitArray)
