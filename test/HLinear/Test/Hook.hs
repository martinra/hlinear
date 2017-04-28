module HLinear.Test.Hook
where

import Test.Tasty

import qualified HLinear.Test.Hook.EchelonForm as EF
import qualified HLinear.Test.Hook.EchelonTransformation as EFT
import qualified HLinear.Test.Hook.LeftTransformation as LT


tests :: TestTree
tests =
  testGroup "Hook Tests"
  [ EF.properties

  , EFT.unitTests
  , EFT.properties

  , LT.unitTests
  , LT.properties
  ]
