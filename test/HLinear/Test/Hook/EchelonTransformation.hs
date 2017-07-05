{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified HLinear.Hook.EchelonTransformation as ET
import HLinear.Matrix ( Matrix(..), IsMatrix(..), Column(..) )
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

properties
  :: forall ctx
  .  ReifiesNModContext ctx
  => Reader (Proxy ctx) TestTree
properties = pure $
  testGroup "Echelon Transformation Properties" $
    [ testPropertyQC "*. . toMatrix == *. on vectors" $
        \et v -> matrixActionOnTopVector
                   (nmbRows et) (et :: EchelonTransformation (NMod ctx))
                   (v :: M.Column (NMod ctx))
    , testPropertyQC "*. . splitAt ==  *. on vectors" $
        \et v n ->
          let (etLeft,etRight) = ET.splitAt n (et :: EchelonTransformation FMPQ)
          in     nmbRows etLeft == fromIntegral (max 0 n)
              && et *. (v :: M.Column FMPQ) == etRight *. (etLeft *. v)
    , testPropertyQC "* . toMatrix == toMatrix . *" $
        \et et'-> multiplicationAsTopMatrix
                   (et :: EchelonTransformation (NMod ctx))
                   (et' :: EchelonTransformation (NMod ctx))
    ]
    <>
    runTestsQC
    [ isMultiplicativeGroup
        ( Proxy :: Proxy (EchelonTransformation (NMod ctx)) )
    , isMultiplicativeLeftAction
        ( Proxy ::  Proxy (EchelonTransformation (NMod ctx)) )
        ( Proxy ::  Proxy (M.Column (NMod ctx)) )
    ]
    <>
    [ testPropertyQC "toMatrix * toInverseMatrix" $
        \et -> let m = toMatrix (et :: EchelonTransformation (NMod ctx)) :: Matrix (NMod ctx)
                   mi = toMatrix $ recip et
                   nrs = fromIntegral $ nmbRows et
               in M.isOne $ m * mi
    ]


matrixActionOnTopVector
  :: forall a b
   . ( Eq b, DecidableZero b, Rng b, IsMatrix a b
     , MultiplicativeSemigroupLeftAction a (Column b) )
  => Natural -> a -> Column b -> Bool
matrixActionOnTopVector (fromIntegral -> nrs) a c@(Column v)
  | nv <= nrs =
      let vzero = V.replicate (nrs-nv) zero
          m = toMatrix a :: Matrix b
          mv = fromColumn $ m *. Column (v <> vzero)
          (mvt, mvb) = V.splitAt nv mv
      in  mvt == fromColumn (a *. c) && all isZero mvb
  | otherwise =
      let (vt,vb) = V.splitAt nrs v
          m = toMatrix a :: Matrix b
      in  fromColumn (m *. Column vt) <> vb == fromColumn (a *. c)
  where
    nv = V.length v

multiplicationAsTopMatrix
  :: forall a
  .  ( Eq a, IsMatrix (EchelonTransformation a) a, Ring a )
  => EchelonTransformation a -> EchelonTransformation a -> Bool
multiplicationAsTopMatrix a a' =
  let convert a
        | r == rmax = toMatrix a :: Matrix a
        | otherwise = toMatrix a <> M.one rdiff
        where
          r = nmbRows a
          rdiff = rmax P.- r
      rmax = max (nmbRows a) (nmbRows a')
  in convert a * convert a' == convert (a * a')
