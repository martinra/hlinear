{-# LANGUAGE ScopedTypeVariables #-}

module HLinear.Test.NormalForm.FoldUnfold.RREF
where

import HLinear.Utility.Prelude

import Math.Structure.Tasty
import Test.Tasty

import HLinear.Matrix ( Matrix, toMatrices )
import HLinear.NormalForm.RREF ( rref )


properties
  :: forall ctx
  .  ReifiesNModContext ctx
  => Reader (Proxy ctx) TestTree
properties = pure $
  testGroup "Reduced row echelon form properties"
  [ testPropertyQSnC 2
    "recombine over division rings" $
    \m -> let [p,l,u,e] = toMatrices $ rref (m :: Matrix (NMod ctx))
          in  m == p * l * u * e
  ]
