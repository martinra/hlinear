{-# LANGUAGE ScopedTypeVariables #-}

module HLinear.Test.NormalForm.FoldUnfold.RREF
where

import HLinear.Utility.Prelude

import Math.Structure.Tasty
import Test.Tasty

import HLinear.Hook.PLEHook ( PLUEHook(..) )
import HLinear.Matrix ( Matrix, toMatrices )
import HLinear.Utility.Fraction ( Fraction(..), fromDenominator )
import qualified HLinear.NormalForm.FoldUnfold.RREF.DivisionRing as DR
import qualified HLinear.NormalForm.FoldUnfold.RREF.FractionFree as FF


properties
  :: forall ctx
  .  ReifiesNModContext ctx
  => Reader (Proxy ctx) TestTree
properties = pure $
  testGroup "Reduced row echelon form properties"
  [ testPropertyQSnC 2
    "recombine over division rings" $
    \m -> let [p,l,u,e] = toMatrices $ DR.rref (m :: Matrix (NMod ctx))
          in  m == p * l * u * e
  , testPropertyQC
    "compare fraction free" $
    \m -> let Fraction e d = FF.rref (m :: Matrix FMPQ)
              PLUEHook _ _ _ e' = DR.rref m
          in  e' == fmap ((fromDenominator d *) . fromIntegral) e
  ]
