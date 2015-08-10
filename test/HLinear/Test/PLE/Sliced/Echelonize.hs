{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  #-}

module HLinear.Test.PLE.Sliced.Echelonize
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import qualified Data.Vector as V
import Data.Maybe
import Data.Proxy
import HFlint.FMPQ
import HFlint.FMPZ
import Math.Structure
import Math.Structure.Tasty
import Numeric.Natural

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Vector

import HLinear.Matrix as M

import HLinear.PLE.Sliced.Echelonize 
import HLinear.PLE.Sliced.Echelonize.DivisionRing
import HLinear.PLE.Decomposition as Decomp
import HLinear.PLE.Hook as Hook
import HLinear.PLE.Strategy.Definition ( PLEStrategy )
import HLinear.PLE.Strategy.FMPQ

import HLinear.Test.Utils as TU


pleProperties :: TestTree
pleProperties =
  testGroup "SlicedPLE decomposition"
  [ testGroup "slicingPositions"
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

  , testPropertyMatrix "recombine ple decomposition" $
      recombinePLE $ PLEDecompositionSlicedParameters
                       SlicingBalanced (SlicingNmb 5)
  ]


testSlicingPositions :: PLEDecompositionSlicedParameters -> Natural -> Bool
testSlicingPositions param n = checkLength && checkConsistence
  where
  ps = slicingPositions param $ fromIntegral n
  checkLength = fromIntegral n == V.sum (V.map snd ps)
  checkConsistence =
    V.all (\(ix,ix',l) -> ix < ix' && ix'-ix == l) $
    V.zip3 (V.map fst ps) (V.tail $ V.map fst ps) (V.map snd ps)

recombinePLE
  :: PLEDecompositionSlicedParameters
  -> Matrix FMPQ -> Bool
recombinePLE param m =
    let (pm,lm,em) = Decomp.toMatrices $
                       pleDecompositionSliced param PLEStrategyFMPQFoldUnfold m
        nrs = M.nmbRows m
        ncs = M.nmbCols m
    in  nrs > 30 || ncs > 30 || m == pm * (lm * em)
