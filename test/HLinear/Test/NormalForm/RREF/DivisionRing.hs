{-# LANGUAGE
    FlexibleContexts
  , Rank2Types
  , ScopedTypeVariables
  #-}

module HLinear.Test.NormalForm.RREF.DivisionRing
where

import HLinear.Utility.Prelude
import qualified Prelude as P

import Math.Structure.Tasty
import Test.Tasty

import HLinear.Hook.PLEHook ( PLREHook(..) )
import HLinear.Matrix ( Matrix, IsMatrix(..) )
import HLinear.NormalForm.RREF
import HLinear.Utility.NmbRowColumn ( nmbRows )
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Matrix as M


properties :: TestTree
properties =
  testGroup "Reduced row echelon form over division rings"
  [ testPropertyQSnC 2 "recombine rref (small prime)" $
      recombineRREF 3

  , testPropertyQSnC 2 "recombine rref (large prime)" $
      recombineRREF 1125899906842679
  ]

recombineRREF :: FlintLimb -> Matrix FMPZ -> Bool
recombineRREF p m =
  withNModContext p $ \(_ :: ReifiesNModContext ctx => Proxy ctx) ->
    let mNMod = fmap toNMod m :: Matrix (NMod ctx)
        PLREHook p l r e = rref mNMod
        im = M.one (nmbRows e P.- nmbRows r) :: Matrix (NMod ctx)
    in  toMatrix p * toMatrix l * toMatrix r * toMatrix e == mNMod
