module HLinear.Test.PLE.Hook.LeftTransformation
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Math.Structure

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import HLinear.PLE.Hook
import HLinear.PLE.Hook.LeftTransformation
import HLinear.PLE.Hook.LeftTransformation.VVMatrix ()
import HLinear.VVMatrix hiding ( nmbRows )


leftTransformationTests :: TestTree
leftTransformationTests =
  testGroup "LeftTransformation"
  [ QC.testProperty "toMatrix * toInverseMatrix" $
      \lt -> let m = toMatrix (lt :: LeftTransformation Rational)
                 mi = toInverseMatrix lt
             in isOne ( m * mi )
  -- QC.testProperty "lt *. toMatrix lt equals identity" $
  --    \lt -> isOne ( lt *. (toMatrix (lt :: LeftTransformation Rational)) )
  ]
