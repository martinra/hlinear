{-# LANGUAGE
    ScopedTypeVariables
  #-}

module HLinear.Test.PLE.FoldUnfold.DivisionRing
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

import HLinear.PLE.FoldUnfold.DivisionRing ( splitOffHook )
import HLinear.PLE.HasPLE
import HLinear.PLE.Hook

import HLinear.Test.Utils


pleProperties :: TestTree
pleProperties =
  testGroup "FoldUnfold PLE decomposition"
  [ testPropertyMatrix "recombine splitOffHook" $
      \m -> fromMaybe True $ do
              let _ = m :: Matrix FMPQ
              (PLEHook p l e, m') <- splitOffHook m

              let m'zero = if M.nmbRows m' == M.nmbRows m
                           then M.zero (M.nmbRows m) 1 `M.blockSumRows` m'
                           else M.zero 1 1 `mappend` m'

              return $ m == (toMatrix p) * (toMatrix l) * (m'zero + toMatrix e)

  , testPropertyMatrix "recombine ple decomposition (small prime)" $
      recombinedPLE 3
      
  , testPropertyMatrix "recombine ple decomposition (large prime)" $
      recombinedPLE 1125899906842679
  ]

recombinedPLE :: FlintLimb -> Matrix FMPZ -> Bool
recombinedPLE p m =
  withNModContext p $ \(_ :: Proxy ctx) ->
    let mNMod = fmap toNMod m :: Matrix (NMod ctx)
        PLEHook p l e = ple mNMod
    in  mNMod == toMatrix p * (toMatrix l * toMatrix e)
