module HLinear.Test.Hook.EchelonForm
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy
import HFlint.FMPQ
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
    
