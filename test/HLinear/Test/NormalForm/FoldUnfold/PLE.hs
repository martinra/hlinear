{-# LANGUAGE ScopedTypeVariables #-}

module HLinear.Test.NormalForm.FoldUnfold.PLE
where

import HLinear.Utility.Prelude

import Math.Structure.Tasty
import Test.Tasty

import HLinear.Hook.EchelonForm ( EchelonForm(..), normalize )
import HLinear.Hook.PLEHook ( PLEHook(..) )
import HLinear.Matrix ( Matrix, toMatrix, toMatrices )
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
  , testPropertyQSnC 2
    "compare fraction free" $
    \m -> let e = FF.ple (m :: Matrix FMPQ)
              PLEHook _ _ e' = DR.ple m
          in  e' == normalize (fmap fromIntegral e :: EchelonForm FMPQ)
  ]
