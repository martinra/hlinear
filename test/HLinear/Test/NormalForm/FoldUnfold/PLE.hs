{-# LANGUAGE ScopedTypeVariables #-}

module HLinear.Test.NormalForm.FoldUnfold.PLE
where

import HLinear.Utility.Prelude

import Math.Structure.Tasty
import Test.Tasty

import HLinear.Hook.PLEHook ( PLEHook(..) )
import HLinear.Matrix ( Matrix, toMatrix )
import qualified HLinear.NormalForm.FoldUnfold.PLE.DivisionRing as DR
import qualified HLinear.NormalForm.FoldUnfold.PLE.FractionFree as FF


properties
  :: forall ctx
  .  ReifiesNModContext ctx
  => Reader (Proxy ctx) TestTree
properties = pure $
  testGroup "FoldUnfold-PLE properties"
  [ testPropertyQSnC 2
    "recombine over division rings" $
    \m -> let PLEHook p l e = DR.ple (m :: Matrix (NMod ctx))
          in  m == toMatrix p * toMatrix l * toMatrix e
  , testPropertySnC 2
    "recombine fraction free" $
    \m -> let PLEHook p l e = FF.ple (m :: Matrix FMPQ)
          in  m == toMatrix p * toMatrix l * toMatrix e
  ]
