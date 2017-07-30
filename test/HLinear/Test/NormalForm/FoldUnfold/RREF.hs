{-# LANGUAGE ScopedTypeVariables #-}

module HLinear.Test.NormalForm.FoldUnfold.RREF
where

import HLinear.Utility.Prelude

import Math.Structure.Tasty
import Test.Tasty

import HLinear.Hook.PLEHook ( PLUEHook(..) )
import HLinear.Matrix ( Matrix, toMatrices )
import HLinear.Matrix.Sized ( MatrixSized(..) )
import HLinear.Utility.Fraction ( Fraction(..), fromFraction )
import qualified HLinear.NormalForm.FoldUnfold.RREF.DivisionRing as DR
import qualified HLinear.NormalForm.FoldUnfold.RREF.FractionFree as FF


properties
  :: forall ctx
  .  ReifiesNModContext ctx
  => Reader (Proxy ctx) TestTree
properties = pure $
  testGroup "Reduced row echelon form"
  [ testPropertyQSnC 2
    "recombine rref over division rings" $
    \m -> let [p,l,u,e] = toMatrices $ DR.rref (m :: Matrix (NMod ctx))
          in  m == p * l * u * e
  , testPropertyQC
    "compare fraction free rref" $
    \msz ->
       let m = fromMatrixSized (msz :: MatrixSized 3 4 FMPQ)
           Fraction e den = FF.rref m
           PLUEHook _ _ _ e' = DR.rref m
       in  e' == fmap (fromFraction . (`Fraction` den) . fromIntegral) e
  ]
