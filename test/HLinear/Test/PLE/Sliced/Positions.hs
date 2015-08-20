{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  #-}

module HLinear.Test.PLE.Sliced.Positions
where

import qualified Data.Vector as V
import Numeric.Natural

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Vector

import HLinear.PLE.Sliced.Echelonize 
import HLinear.PLE.Sliced.Echelonize.Positions

import HLinear.Test.Utils as TU


positionProperties :: TestTree
positionProperties =
  testGroup "slicingPositions"
  [ TU.testProperty "Unbalanced Number" $ \s ->
      testSlicingPositions $
        PLEDecompositionSlicedParameters
          SlicingUnBalanced (SlicingNmb $ succ s)

  , TU.testProperty "Unbalanced Size" $ \s ->
      testSlicingPositions $
        PLEDecompositionSlicedParameters
          SlicingUnBalanced (SlicingSize $ succ s)
  
  , TU.testProperty "Balanced Number" $ \s ->
      testSlicingPositions $
        PLEDecompositionSlicedParameters
          SlicingBalanced (SlicingNmb $ succ s)
  
  , TU.testProperty "Balanced Size" $ \s ->
      testSlicingPositions $
        PLEDecompositionSlicedParameters
          SlicingBalanced (SlicingSize $ succ s)
  ]

testSlicingPositions :: PLEDecompositionSlicedParameters -> Natural -> Bool
testSlicingPositions param n = checkLength && checkConsistence
  where
  ps = slicingPositions param $ fromIntegral n
  checkLength = fromIntegral n == V.sum (V.map snd ps)
  checkConsistence =
    V.all (\(ix,ix',l) -> ix < ix' && ix'-ix == l) $
    V.zip3 (V.map fst ps) (V.tail $ V.map fst ps) (V.map snd ps)

