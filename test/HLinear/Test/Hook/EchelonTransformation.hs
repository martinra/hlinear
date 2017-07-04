{-# LANGUAGE ViewPatterns #-}

module HLinear.Test.Hook.EchelonTransformation
where

import HLinear.Utility.Prelude
import qualified Prelude as P

import Data.Ratio ( (%), Rational )
import Math.Structure.Tasty
import Test.Tasty
import Test.Tasty.HUnit ( testCase, (@?=) )
import qualified Data.Vector as V

import HLinear.Hook.EchelonTransformation ( EchelonTransformation(..), EchelonTransformationColumn(..) )
import HLinear.Matrix ( Matrix(..), toMatrix )
import HLinear.Utility.NmbRowColumn ( nmbRows )
import qualified HLinear.Matrix as M


unitTests :: TestTree
unitTests =
  testGroup "Echelon Transformation Unit Tests"
  [ testCase "toMatrix trivial" $
      let et = EchelonTransformation 2 V.empty :: EchelonTransformation Rational
      in  toMatrix et @?= M.fromLists
                              ([[1,0], [0,1]] :: [[Rational]])

  , testCase "toMatrix diagonal" $
      let et = EchelonTransformation 2 $ V.fromList
                 [ EchelonTransformationColumn 0 $ V.fromList [0]
                 , EchelonTransformationColumn 1 V.empty
                 ] 
                 :: EchelonTransformation Rational
      in  toMatrix et @?= M.fromLists
                              ([[1,0], [0,1]] :: [[Rational]])

  , testCase "toMatrix general" $
      let et = EchelonTransformation 3 $ V.fromList
                 [ EchelonTransformationColumn 0 $ V.fromList [3%8, 9%14]
                 , EchelonTransformationColumn 1 $ V.fromList [1%7]
                 , EchelonTransformationColumn 2 V.empty
                 ]
                 :: EchelonTransformation Rational
      in  toMatrix et @?= M.fromLists
                              ([[1,1%7,3%8], [0,1,9%14], [0,0,1]] :: [[Rational]])
  ]

properties :: TestTree
properties =
  testGroup "Echelon Transformation Properties" $
    [ testPropertyQC "toMatrix *. vector == *. vector" $
        \et v -> matrixActionOnTopVector
                   (nmbRows et) (et :: EchelonTransformation FMPQ)
                   (v :: M.Column FMPQ)
    ]
    <>
    runTestsQC
    [ isMultiplicativeGroup
        ( Proxy :: Proxy (EchelonTransformation FMPQ) )
    , isMultiplicativeLeftAction
        ( Proxy ::  Proxy (EchelonTransformation FMPQ) )
        ( Proxy ::  Proxy (M.Column FMPQ) )
    ]
    <>
    [ testPropertyQC "toMatrix * toInverseMatrix" $
        \et -> let m = toMatrix (et :: EchelonTransformation FMPQ) :: Matrix FMPQ
                   mi = toMatrix $ recip et
                   nrs = fromIntegral $ nmbRows et
               in M.isOne $ m * mi
    ]


matrixActionOnTopVector
  :: forall a b
   . ( Eq b, IsMatrix a b, Rng b
     , MultiplicativeSemigroupLeftAction a (Column b) )
  => Natural -> a -> Column b -> Bool
matrixActionOnTopVector (nrs <- fromIntegral) a c@(Column v)
  | nv <= nrs =
      let vzero = V.replicate (nrs-nv) zero
          m = toMatrix a :: Matrix b
          mv = fromColumn $ m *. Column (v <> vzero)
          (mvt, mvb) = V.splitAt nrs mv
      in  mvt == fromColumn (a *. c) && all isZero mvb
  | otherwise =
      let (vt,vb) = V.splitAt nrs v
          m = toMatrix a :: Matrix b
      in  (fromColumn $ m *. Column vt) <> vb == fromColumn (a *. c)
  where
    nv = V.length v
