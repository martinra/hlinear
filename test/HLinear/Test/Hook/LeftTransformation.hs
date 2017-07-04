{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module HLinear.Test.Hook.LeftTransformation
where

import HLinear.Utility.Prelude
import qualified Prelude as P

import Data.Ratio ( (%), Rational )
import Math.Structure.Tasty
import Test.Tasty
import Test.Tasty.HUnit ( testCase, (@?=) )
import qualified Data.Vector as V

import HLinear.Hook.LeftTransformation ( LeftTransformation(..), LeftTransformationColumn(..) )
import HLinear.Matrix ( Matrix(..), IsMatrix(..), Column(..) )
import qualified HLinear.Matrix as M


unitTests :: TestTree
unitTests =
  testGroup "LeftTransformation Unit Tests"
  [ testCase "toMatrix trivial" $
      let lt = LeftTransformation 2 V.empty :: LeftTransformation Rational
      in  M.toMatrix lt @?= M.fromLists
                           ([[1,0], [0,1]] :: [[Rational]])

  , testCase "toMatrix diagonal" $
      let lt = LeftTransformation 2 $ V.fromList
                 [ LeftTransformationColumn 0 (Unit 2) $ V.fromList [0]
                 , LeftTransformationColumn 1 (Unit (1%3)) V.empty ]
                 :: LeftTransformation Rational
      in  M.toMatrix lt @?= M.fromLists
                           ([[2,0], [0,1%3]] :: [[Rational]])

  , testCase "toMatrix unipotent" $
      let lt = LeftTransformation 3 $ V.fromList
                 [ LeftTransformationColumn 0 (Unit 1) $ V.fromList [3%8, 9%14]
                 , LeftTransformationColumn 1 (Unit 1) $ V.fromList [1%7]
                 , LeftTransformationColumn 2 (Unit 1) V.empty
                 ]
                 :: LeftTransformation Rational
      in  M.toMatrix lt @?= M.fromLists
                           ([[1,0,0], [3%8,1,0], [9%14,1%7,1]] :: [[Rational]])

  , testCase "toMatrix general" $
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
    <>
    [ testPropertyQC "toMatrix *. vector == *. vector" $
        \lt v -> matrixActionOnBottomVector
                   (nmbRows lt) (lt :: LeftTransformation FMPQ)
                   (v :: M.Column FMPQ)
    ]
    <>
    [ testPropertyQC "toMatrix * toInverseMatrix" $
        \lt -> let m = M.toMatrix (lt :: LeftTransformation FMPQ) :: Matrix FMPQ
                   mi = M.toMatrix $ recip lt
                   nrs = fromIntegral $ nmbRows lt
               in m * mi == M.one nrs
    ]


matrixActionOnBottomVector
  :: forall a b
   . ( Eq b, IsMatrix a b, Rng b
     , MultiplicativeSemigroupLeftAction a (Column b) )
  => Natural -> a -> Column b -> Bool
matrixActionOnBottomVector (fromIntegral -> nrs) a c@(Column v)
  | nv <= nrs =
      let v' = V.replicate (nrs-nv) zero <> v
          m = toMatrix a :: Matrix b
      in  m *. Column v' == a *. c
  | otherwise =
      let (vt,vb) = V.splitAt (nv-nrs) v
          m = toMatrix a :: Matrix b
      in  vt <> fromColumn (m *. Column vb) == fromColumn (a *. c)
  where
    nv = V.length v
