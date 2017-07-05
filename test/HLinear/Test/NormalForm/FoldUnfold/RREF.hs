{-# LANGUAGE
    FlexibleContexts
  , Rank2Types
  , ScopedTypeVariables
  #-}

module HLinear.Test.NormalForm.FoldUnfold.RREF
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


properties
  :: forall ctx
  .  ReifiesNModContext ctx
  => Reader (Proxy ctx) TestTree
properties = pure $
  testGroup "Reduced row echelon form properties"
  [ testPropertyQSnC 2
    "recombine over division rings" $
    \m -> let PLREHook p l r e = rref (m :: Matrix (NMod ctx))
          in  m == toMatrix p * toMatrix l * toMatrix r * toMatrix e
  ]
