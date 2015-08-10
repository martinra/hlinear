{-# LANGUAGE
    ScopedTypeVariables
  #-}

module HLinear.Test.PLE.FoldUnfold.Echelonize
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

import HLinear.PLE.FoldUnfold.Echelonize 
import HLinear.PLE.FoldUnfold.Echelonize.DivisionRing
import HLinear.PLE.Decomposition as Decomp
import HLinear.PLE.Hook as Hook

import HLinear.Test.Utils


pleProperties :: TestTree
pleProperties =
  testGroup "FoldUnfold PLE decomposition"
  [ testPropertyMatrix "recombine splitOffHook" $
      \m -> fromMaybe True $ do
              let _ = m :: Matrix FMPQ
              (pleHook, m') <- splitOffHook m

              let (pm,lm,em) = Hook.toMatrices pleHook
              let m'zero = if M.nmbRows m' == M.nmbRows m
                           then M.zeroMatrix (M.nmbRows m) 1 `M.blockSumRows` m'
                           else M.zeroMatrix 1 1 `mappend` m'

              return $ m == pm * lm * (m'zero + em)

  , testPropertyMatrix "recombine ple decomposition (small prime)" $
      recombinedPLE 3
      
  , testPropertyMatrix "recombine ple decomposition (large prime)" $
      recombinedPLE 1125899906842679
  ]

recombinedPLE :: FlintLimb -> Matrix FMPZ -> Bool
recombinedPLE p m =
  withNModContext p $ \(_ :: Proxy ctx) ->
    let mNMod = fmap toNMod m :: Matrix (NMod ctx)
        (pm,lm,em) = Decomp.toMatrices $
                       pleDecompositionFoldUnfold mNMod
    in  mNMod == pm * (lm * em)
