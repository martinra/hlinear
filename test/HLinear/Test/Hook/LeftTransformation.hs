module HLinear.Test.PLE.Hook.LeftTransformation
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy
import Data.Ratio ( (%) )
import Math.Structure
import Math.Structure.Tasty
import qualified Data.Vector as V

import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ( (@?=) )
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import Test.Vector

import HLinear.PLE.Hook
import HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.Matrix ( Matrix(..) )
import qualified HLinear.Matrix as M

import HLinear.Test.Utility.Misc


unitTests :: TestTree
unitTests =
  testGroup "LeftTransformation Unit Tests"
  [ HU.testCase "toMatrix trivial" $
      let lt = LeftTransformation 2 V.empty :: LeftTransformation Rational
      in  M.toMatrix lt @?= M.fromListsUnsafe
                           ([[1,0], [0,1]] :: [[Rational]])

  , HU.testCase "toMatrix diagonal" $
      let lt = LeftTransformation 2 $ V.fromList
                 [ LeftTransformationColumn 0 (nonZero 2) $ V.fromList [0]
                 , LeftTransformationColumn 1 (nonZero (1%3)) V.empty ]
                 :: LeftTransformation Rational
      in  M.toMatrix lt @?= M.fromListsUnsafe
                           ([[2,0], [0,1%3]] :: [[Rational]])

  , HU.testCase "toMatrix unipotent" $
      let lt = LeftTransformation 3 $ V.fromList
                 [ LeftTransformationColumn 0 (nonZero 1) $ V.fromList [3%8, 9%14]
                 , LeftTransformationColumn 1 (nonZero 1) $ V.fromList [1%7]
                 , LeftTransformationColumn 2 (nonZero 1) V.empty
                 ]
                 :: LeftTransformation Rational
      in  M.toMatrix lt @?= M.fromListsUnsafe
                           ([[1,0,0], [3%8,1,0], [9%14,1%7,1]] :: [[Rational]])

  , HU.testCase "toMatrix general" $
      let lt = LeftTransformation 3 $ V.fromList
                 [ LeftTransformationColumn 0 (nonZero $ 7%5) $ V.fromList [3%8, 9%14]
                 , LeftTransformationColumn 1 (nonZero $ 3%2) $ V.fromList [1%7]
                 , LeftTransformationColumn 2 (nonZero $ 8%14) V.empty
                 ]
                 :: LeftTransformation Rational
      in  M.toMatrix lt @?= M.fromListsUnsafe
                           ([[7%5,0,0], [21%40,3%2,0], [9%10,3%14,8%14]] :: [[Rational]])
  ]

properties :: TestTree
properties =
  testGroup "LeftTransformation Properties" $
    testAlgebraicStructureQC
    [ isMultiplicativeGroup
        ( Proxy :: Proxy (LeftTransformation Rational) )
    , isMultiplicativeLeftAction
        ( Proxy ::  Proxy (LeftTransformation Rational) )
        ( Proxy ::  Proxy (Vector Rational) )
    ]
    ++
    [ QC.testProperty "toMatrix *. vector == *. vector" $
        \lt v -> matrixActionOnBottomVector
                   (LT.nmbRows lt) (lt :: LeftTransformation Rational)
                   (v :: Vector Rational)
    ]
    ++
    [ testPropertyMatrixSC "toMatrix * toInverseMatrix" $
        \lt -> let m = M.toMatrix (lt :: LeftTransformation Rational) :: Matrix Rational
                   mi = M.toMatrix $ recip lt
                   nrs = fromIntegral $ LT.nmbRows lt
               in m * mi == M.one nrs
    ]
