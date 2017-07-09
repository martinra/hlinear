{-# LANGUAGE ScopedTypeVariables #-}

module HLinear.Test.NormalForm.FoldUnfold.PLE
where

import HLinear.Utility.Prelude

import Math.Structure.Tasty
import Test.Tasty

import HLinear.Matrix ( Matrix, toMatrices )
import qualified HLinear.NormalForm.FoldUnfold.PLE.DivisionRing as DR
import qualified HLinear.NormalForm.FoldUnfold.PLE.FractionFree as FF


properties
  :: forall ctx
  .  ReifiesNModContext ctx
  => Reader (Proxy ctx) TestTree
properties = pure $
  testGroup "FoldUnfold-PLE properties"
  [ testPropertyQSnC 3
    "recombine over division rings" $
    \m -> let [p,l,e] = toMatrices $ DR.ple (m :: Matrix (NMod ctx))
          in  m == p * l * e
  , testPropertySnC 2
    "recombine fraction free" $
    \m -> let [p,l,e] = toMatrices $ FF.ple (m :: Matrix FMPQ)
          in  m == p * l * e
  ]
