{-# LANGUAGE
    FlexibleContexts
  #-}

module HLinear.Test.PLE.MultiMod.Echelonize
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Maybe
import Data.Proxy
import HFlint.FMPQ
import HFlint.FMPZ
import HFlint.NMod
import Math.Structure
import Math.Structure.Tasty

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Vector

import HLinear.Matrix as M
import HLinear.MultiMod ( ReconstructionParameters(..) )

import HLinear.PLE.MultiMod.Echelonize 
import HLinear.PLE.MultiMod.Echelonize.FMPQ
import HLinear.PLE.Decomposition as Decomp
import HLinear.PLE.Hook as Hook
import HLinear.PLE.Strategy.Definition ( PLEStrategy )
import HLinear.PLE.Strategy.NMod

import HLinear.Test.Utils


pleProperties :: TestTree
pleProperties =
  testGroup "MultiMod PLE decomposition"
  [ testPropertyMatrix "recombine ple decomposition" $
      \mFMPZ ->
        let m = fmap (`fromFMPZs` 1) mFMPZ :: Matrix FMPQ
            nrs = M.nmbRows m
            ncs = M.nmbCols m

            param = PLEDecompositionMultiModParameters $
                      ReconstructionParameters 20 10
            strat
              :: ReifiesNModContext ctx
              => Proxy ctx -> PLEStrategy Maybe (Matrix (NMod ctx))
            strat = const $ PLEStrategyNModLiftIdenity PLEStrategyNModFoldUnfold
            (pm,lm,em) = Decomp.toMatrices $
                           pleDecompositionMultiMod param strat m
        in nrs > 30 || ncs > 30 || m == pm * (lm * em)
  ]
