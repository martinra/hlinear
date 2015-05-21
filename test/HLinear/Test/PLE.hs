module HLinear.Test.PLE
where

import Test.Tasty

import HLinear.Test.PLE.Hook ( hookTests )


pleTests :: TestTree
pleTests =
  testGroup "PLE Tests"
  [ hookTests
  ]
