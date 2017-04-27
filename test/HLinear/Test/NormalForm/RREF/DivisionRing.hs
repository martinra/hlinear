{-# LANGUAGE
    FlexibleContexts
  , Rank2Types
  , ScopedTypeVariables
  #-}

module HLinear.Test.PLE.FoldUnfold.ReducedEchelonForm
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )


import Data.Proxy
import Data.Maybe
import qualified Data.Vector as V
import HFlint.FMPQ
import HFlint.FMPZ
import HFlint.NMod
import Math.Structure

import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC

import HLinear.Matrix

import HLinear.PLE.HasPLE
import HLinear.PLE.FoldUnfold.ReducedEchelonForm
import HLinear.PLE.Hook
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm.Container ()
import HLinear.PLE.Hook.EchelonTransformation as ET
import qualified HLinear.Matrix as M

import HLinear.Test.Utils

import Debug.Trace


properties :: TestTree
properties =
  testGroup "Reduced Row Echelon Form Properties"
  [ testPropertyMatrix "recombine reducedEchelonForm (small prime)" $
      recombineRREF 3

  , testPropertyMatrix "recombine reducedEchelonForm (large prime)" $
      recombineRREF 1125899906842679
          ]

recombineRREF :: FlintLimb -> Matrix FMPZ -> Bool
recombineRREF p m =
  withNModContext p $ \(_ :: ReifiesNModContext ctx => Proxy ctx) ->
    let mNMod = fmap toNMod m :: Matrix (NMod ctx)
        PLREHook p l r e = rref mNMod
        im = M.one (EF.nmbRows e P.- ET.nmbRows r) :: Matrix (NMod ctx)
    in  toMatrix p * toMatrix l * toMatrix r * toMatrix e == mNMod
