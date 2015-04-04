module PLETests
where

import Test.Tasty

import PLETests.VVMatrixField ( vvMatrixFieldTests )


pleTests :: TestTree
pleTests =
  testGroup "PLE Tests"
  [ vvMatrixFieldTests
  ]
