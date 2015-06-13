module HLinear.Test.PLE.Hook.EchelonForm
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy
import Math.Structure.Tasty

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Vector

import HLinear.PLE.Hook.EchelonForm as EF

import HLinear.Test.Utils


echelonFormProperties :: TestTree
echelonFormProperties =
  testGroup "EchelonForm" $
    ( (`runTestR` QC.testProperty ) $
      fmap concat $ sequence
      [ isAdditiveSemigroup
          ( Proxy :: Proxy (EchelonForm Rational) )
      ]
    ) 
