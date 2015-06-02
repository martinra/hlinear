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
import qualified Test.Tasty.QuickCheck as QC
import Test.Vector

import HLinear.PLE.Hook
import HLinear.PLE.Hook.LeftTransformation
import HLinear.PLE.Hook.LeftTransformation.VVMatrix ()
import qualified HLinear.VVMatrix as M


leftTransformationTests :: TestTree
leftTransformationTests =
  testGroup "LeftTransformation" $
  [ leftTransformationUnitTests
  ]

leftTransformationUnitTests :: TestTree
leftTransformationUnitTests =
  testGroup "Unit Tests" $
  [ HU.testCase "toMatrix trivial" $
      let lt = LeftTransformation 2 $ V.empty
      in toMatrix lt @?= M.fromLists ([[1,0], [0,1]] :: [[Rational]])

  , HU.testCase "toMatrix diagonal" $
      let lt = LeftTransformation 2 $ V.fromList [ LeftTransformationColumn 0 (nonZero 2) $ V.fromList [0] , LeftTransformationColumn 1 (nonZero (1%3)) $ V.empty ]
      in toMatrix lt @?= M.fromLists ([[2,0], [0,1%3]] :: [[Rational]])

  , HU.testCase "toMatrix unipotent" $
      let lt = LeftTransformation 3 $ V.fromList
                 [ LeftTransformationColumn 0 (nonZero 1) $ V.fromList [3%8, 9%14]
                 , LeftTransformationColumn 1 (nonZero 1) $ V.fromList [1%7]
                 , LeftTransformationColumn 2 (nonZero 1) $ V.empty
                 ]
      in toMatrix lt @?= M.fromLists ([[1,0,0], [3%8,1,0], [9%14,1%7,1]] :: [[Rational]])

  , HU.testCase "toMatrix general" $
      let lt = LeftTransformation 3 $ V.fromList
                 [ LeftTransformationColumn 0 (nonZero $ 7%5) $ V.fromList [3%8, 9%14]
                 , LeftTransformationColumn 1 (nonZero $ 3%2) $ V.fromList [1%7]
                 , LeftTransformationColumn 2 (nonZero $ 8%14) $ V.empty
                 ]
      in toMatrix lt @?= M.fromLists ([[7%5,0,0], [21%40,3%2,0], [9%10,3%14,8%14]] :: [[Rational]])
  ]

-- leftTransformationTests :: TestTree
-- leftTransformationTests =
--   testGroup "LeftTransformation" $
--     [ QC.testProperty "toMatrix *. vector == *. vector" $
--         \lt v -> let m = toMatrix (lt :: LeftTransformation Rational)
--                      nrsDiff = V.length v - fromIntegral (nmbRows lt)
--                      (v1,v2) = V.splitAt nrsDiff v
--                      mv2 = m *. ( V.replicate (-nrsDiff) 0 V.++ v2 )
--                      mv = v1 V.++ V.drop (-nrsDiff) mv2
--                  in lt *. (v :: V.Vector Rational) == mv
--     ]
--  (`runTestR` QC.testProperty) $
--  isMultiplicativeSemigroup (Proxy :: Proxy (LeftTransformation Rational))
  
--  [ QC.testProperty "toMatrix * toInverseMatrix" $
--      \lt -> let m = toMatrix (lt :: LeftTransformation Rational)
--                 mi = toInverseMatrix lt
--             in isOne ( m * mi )
--  -- QC.testProperty "lt *. toMatrix lt equals identity" $
--  --    \lt -> isOne ( lt *. (toMatrix (lt :: LeftTransformation Rational)) )
--  ]
