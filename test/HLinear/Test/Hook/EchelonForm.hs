module HLinear.Test.Hook.EchelonForm
where

import HLinear.Utility.Prelude

import Math.Structure.Tasty
import Test.Tasty

import HLinear.Hook.EchelonForm


properties :: TestTree
properties =
  testGroup "Echelon Form Properties" $
    runTestsQC
    [ isAbelianSemigroup
        ( Proxy :: Proxy (EchelonForm FMPQ) )
    ]
    
