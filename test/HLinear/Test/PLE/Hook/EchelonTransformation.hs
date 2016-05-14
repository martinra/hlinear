module HLinear.Test.PLE.Hook.EchelonTransformation
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
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ( (@?=) )
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import Test.Vector

import HLinear.PLE.Hook
import HLinear.PLE.Hook.EchelonTransformation as ET
import HLinear.PLE.Hook.EchelonTransformation.Column as ETC
import qualified HLinear.Matrix as M

import HLinear.Test.Utils


echelonTransformationTests :: TestTree
echelonTransformationTests =
  testGroup "EchelonTransformation"
  [ echelonTransformationUnitTests
  , echelonTransformationProperties
  ]

echelonTransformationUnitTests :: TestTree
echelonTransformationUnitTests =
  testGroup "Unit Tests"
  [ HU.testCase "toMatrix trivial" $
      let et = EchelonTransformation 2 V.empty
      in toMatrix et @?= M.fromListsUnsafe
                           ([[1,0], [0,1]] :: [[Rational]])

  , HU.testCase "toMatrix diagonal" $
      let et = EchelonTransformation 2 $ V.fromList
                 [ EchelonTransformationColumn 0 $ V.fromList [0]
                 , EchelonTransformationColumn 1 V.empty ]
      in toMatrix et @?= M.fromListsUnsafe
                           ([[1,0], [0,1]] :: [[Rational]])

  , HU.testCase "toMatrix general" $
      let et = EchelonTransformation 3 $ V.fromList
                 [ EchelonTransformationColumn 0 $ V.fromList [3%8, 9%14]
                 , EchelonTransformationColumn 1 $ V.fromList [1%7]
                 , EchelonTransformationColumn 2 V.empty
                 ]
      in toMatrix et @?= M.fromListsUnsafe
                           ([[1,1%7,3%8], [0,1,9%14], [0,0,1]] :: [[Rational]])
  ]

echelonTransformationProperties :: TestTree
echelonTransformationProperties =
  testGroup "Properties" $
    [ QC.testProperty "toMatrix *. vector == *. vector" $
        \et v -> let m = toMatrix (et :: EchelonTransformation Rational)
                     nv = V.length v
                     nrsZ = fromIntegral $ nmbRows et
                     nrsDiff = nv - nrsZ
                     (v1,v2) = V.splitAt nrsZ v
                     mv1 = m *. ( v1 V.++ V.replicate (-nrsDiff) 0 )
                     mv = V.take nv mv1 V.++ v2
                 in V.all (==0) (V.drop nv mv1)
                    &&
                    et *. (v :: V.Vector Rational) == mv

    , QC.testProperty "*. EchelonTransformation Column" $
        \et etc -> let _ = et :: EchelonTransformation Rational
                       _ = etc :: EchelonTransformationColumn Rational
                   in toVector (et *. etc) == et *. toVector etc
    ]
    ++
    ( (`runTestR` testPropertyMatrixSC ) $
      fmap concat $ sequence
      [ isMultiplicativeGroup
          ( Proxy :: Proxy (EchelonTransformation Rational) )
      , isMultiplicativeLeftAction
          ( Proxy ::  Proxy (EchelonTransformation Rational) )
          ( Proxy ::  Proxy (Vector Rational) )
      ]
    )
    ++
    [ testPropertyMatrixSC "toMatrix * toInverseMatrix" $
        \et -> let m = toMatrix (et :: EchelonTransformation Rational)
                   mi = toInverseMatrix et
                   nrs = fromIntegral $ ET.nmbRows et
               in m * mi == M.identityMatrix nrs
    ,
      testPropertyMatrixSC "et *. toInverseMatrix et equals identity" $
        \et -> let mi = toInverseMatrix (et :: EchelonTransformation Rational)
                   nrs = fromIntegral $ ET.nmbRows et
               in et *. mi == M.identityMatrix nrs
    ]
