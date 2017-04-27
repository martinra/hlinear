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


properties :: TestTree
properties =
  testGroup "PLE Properties"
  [ testPropertyMatrix "recombine ple decomposition (small prime)" $
      recombinePLE 3
      
  , testPropertyMatrix "recombine ple decomposition (large prime)" $
      recombinePLE 1125899906842679
  ]

recombinePLE :: FlintLimb -> Matrix FMPZ -> Bool
recombinePLE p m =
  withNModContext p $ \(_ :: Proxy ctx) ->
    let mNMod = fmap toNMod m :: Matrix (NMod ctx)
        PLEHook p l e = ple mNMod
    in  mNMod == toMatrix p * toMatrix l * toMatrix e
