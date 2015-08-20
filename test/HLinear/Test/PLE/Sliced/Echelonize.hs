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
import HFlint.NMod
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
import HLinear.PLE.Strategy.NMod

import HLinear.Test.Utils as TU


pleProperties :: TestTree
pleProperties =
  testGroup "SlicedPLE decomposition"
  [ testPropertyMatrix "recombine ple decomposition" $
      recombinePLE $ PLEDecompositionSlicedParameters
                       SlicingBalanced (SlicingNmb 5)

  , testPropertyMatrix "recombine ple decomposition (small prime)" $
      recombinePLENMod 3 $ PLEDecompositionSlicedParameters
                         SlicingBalanced (SlicingNmb 5)

  , testPropertyMatrix "recombine ple decomposition (large prime)" $
      recombinePLENMod 1125899906842679 $ PLEDecompositionSlicedParameters
                         SlicingBalanced (SlicingNmb 5)
  ]

recombinePLE
  :: PLEDecompositionSlicedParameters
  -> Matrix FMPQ -> Bool
recombinePLE param m =
    let (pm,lm,em) = Decomp.toMatrices $
                       pleDecompositionSliced param PLEStrategyFMPQFoldUnfold m
        nrs = M.nmbRows m
        ncs = M.nmbCols m
    in  nrs > 30 || ncs > 30 || m == pm * (lm * em)

recombinePLENMod
  :: FlintLimb -> PLEDecompositionSlicedParameters
  -> Matrix FMPZ -> Bool
recombinePLENMod p param m =
  withNModContext p $ \(_::Proxy ctx) ->
    let mNMod = fmap toNMod m :: Matrix (NMod ctx)
        (pm,lm,em) = Decomp.toMatrices $
                       pleDecompositionSliced param PLEStrategyNModFoldUnfold mNMod
    in  mNMod == pm * (lm * em)
