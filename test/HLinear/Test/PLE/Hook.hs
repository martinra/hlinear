module HLinear.Test.PLE.Hook
where

import Test.Tasty

import HLinear.Test.PLE.Hook.EchelonForm
import HLinear.Test.PLE.Hook.LeftTransformation
import HLinear.Test.PLE.Hook.PLE

hookTests :: TestTree
hookTests =
  testGroup "PLEHook Tests"
  [-- leftTransformationTests
   -- echelonFormProperties
   hookPLEProperties
  ]
