{-# LANGUAGE
    FlexibleContexts
  , Rank2Types
  , ScopedTypeVariables
  #-}

module HLinear.Test.NormalForm.RREF.DivisionRing
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy
import HFlint.FMPZ
import HFlint.NMod
import Math.Structure
import Math.Structure.Tasty
import Test.Tasty

import HLinear.Hook.PLEHook ( PLREHook(..) )
import HLinear.Matrix ( Matrix, IsMatrix(..) )
import HLinear.NormalForm.RREF
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Hook.EchelonTransformation as ET
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
        im = M.one (EF.nmbRows e P.- ET.nmbRows r) :: Matrix (NMod ctx)
    in  toMatrix p * toMatrix l * toMatrix r * toMatrix e == mNMod
