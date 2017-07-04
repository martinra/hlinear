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
import HLinear.Test.Utility.Vector
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
                 [ EchelonTransformationColumn 1 V.empty
                 , EchelonTransformationColumn 0 $ V.fromList [0]
                 ] 
                 :: EchelonTransformation Rational
      in  toMatrix et @?= M.fromLists
                              ([[1,0], [0,1]] :: [[Rational]])

  , testCase "toMatrix general" $
      let et = EchelonTransformation 3 $ V.fromList
                 [ EchelonTransformationColumn 2 V.empty
                 , EchelonTransformationColumn 1 $ V.fromList [1%7]
                 , EchelonTransformationColumn 0 $ V.fromList [3%8, 9%14]
                 ]
                 :: EchelonTransformation Rational
      in  toMatrix et @?= M.fromLists
                              ([[1,1%7,3%8], [0,1,9%14], [0,0,1]] :: [[Rational]])
  ]

properties :: TestTree
properties =
  testGroup "Echelon Transformation Properties" $
    runTestsQC
    [ isMultiplicativeGroup
        ( Proxy :: Proxy (EchelonTransformation FMPQ) )
    , isMultiplicativeLeftAction
        ( Proxy ::  Proxy (EchelonTransformation FMPQ) )
        ( Proxy ::  Proxy (M.Column FMPQ) )
    ]
    <>
    [ testPropertyQC "toMatrix *. vector == *. vector" $
        \et v -> matrixActionOnTopVector
                   (nmbRows et) (et :: EchelonTransformation FMPQ)
                   (v :: M.Column FMPQ)
    ]
    <>
    [ testPropertyQC "toMatrix * toInverseMatrix" $
        \et -> let m = toMatrix (et :: EchelonTransformation FMPQ) :: Matrix FMPQ
                   mi = toMatrix $ recip et
                   nrs = fromIntegral $ nmbRows et
               in M.isOne $ m * mi
    ]
