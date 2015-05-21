module HLinear.Test.PLE.Hook
where

import Test.Tasty

import HLinear.Test.PLE.Hook.LeftTransformation


hookTests :: TestTree
hookTests =
  testGroup "PLEHook Tests"
  [ leftTransformationTests
  ]
