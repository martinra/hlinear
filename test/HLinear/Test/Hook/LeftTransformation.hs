module HLinear.Test.Hook.LeftTransformation
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy
import Data.Ratio ( (%) )
import Data.Vector ( Vector )
import HFlint.FMPQ
import Math.Structure
import Math.Structure.Tasty
import qualified Data.Vector as V

import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ( (@?=) )
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import Test.Vector

import HLinear.Hook.LeftTransformation as LT
import HLinear.Matrix ( Matrix(..) )
import qualified HLinear.Matrix as M

import HLinear.Test.Utility.Vector


unitTests :: TestTree
unitTests =
  testGroup "LeftTransformation Unit Tests"
  [ HU.testCase "toMatrix trivial" $
      let lt = LeftTransformation 2 V.empty :: LeftTransformation Rational
      in  M.toMatrix lt @?= M.fromLists
                           ([[1,0], [0,1]] :: [[Rational]])

  , HU.testCase "toMatrix diagonal" $
      let lt = LeftTransformation 2 $ V.fromList
                 [ LeftTransformationColumn 0 (Unit 2) $ V.fromList [0]
                 , LeftTransformationColumn 1 (Unit (1%3)) V.empty ]
                 :: LeftTransformation Rational
      in  M.toMatrix lt @?= M.fromLists
                           ([[2,0], [0,1%3]] :: [[Rational]])

  , HU.testCase "toMatrix unipotent" $
      let lt = LeftTransformation 3 $ V.fromList
                 [ LeftTransformationColumn 0 (Unit 1) $ V.fromList [3%8, 9%14]
                 , LeftTransformationColumn 1 (Unit 1) $ V.fromList [1%7]
                 , LeftTransformationColumn 2 (Unit 1) V.empty
                 ]
                 :: LeftTransformation Rational
      in  M.toMatrix lt @?= M.fromLists
                           ([[1,0,0], [3%8,1,0], [9%14,1%7,1]] :: [[Rational]])

  , HU.testCase "toMatrix general" $
      let lt = LeftTransformation 3 $ V.fromList
                 [ LeftTransformationColumn 0 (Unit $ 7%5) $ V.fromList [3%8, 9%14]
                 , LeftTransformationColumn 1 (Unit $ 3%2) $ V.fromList [1%7]
                 , LeftTransformationColumn 2 (Unit $ 8%14) V.empty
                 ]
                 :: LeftTransformation Rational
      in  M.toMatrix lt @?= M.fromLists
                           ([[7%5,0,0], [21%40,3%2,0], [9%10,3%14,8%14]] :: [[Rational]])
  ]

properties :: TestTree
properties =
  testGroup "LeftTransformation Properties" $
    runTestsQC
    [ isMultiplicativeGroup
        ( Proxy :: Proxy (LeftTransformation FMPQ) )
    , isMultiplicativeLeftAction
        ( Proxy ::  Proxy (LeftTransformation FMPQ) )
        ( Proxy ::  Proxy (M.Column FMPQ) )
    ]
    ++
    [ QC.testProperty "toMatrix *. vector == *. vector" $
        \lt v -> matrixActionOnBottomVector
                   (LT.nmbRows lt) (lt :: LeftTransformation FMPQ)
                   (v :: M.Column FMPQ)
    ]
    ++
    [ QC.testProperty "toMatrix * toInverseMatrix" $
        \lt -> let m = M.toMatrix (lt :: LeftTransformation FMPQ) :: Matrix FMPQ
                   mi = M.toMatrix $ recip lt
                   nrs = fromIntegral $ LT.nmbRows lt
               in m * mi == M.one nrs
    ]
