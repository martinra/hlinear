module HLinear.Test.Hook
where

import HLinear.Utility.Prelude

import Test.Tasty

import qualified HLinear.Test.Hook.EchelonForm as EF
import qualified HLinear.Test.Hook.EchelonTransformation as EFT
import qualified HLinear.Test.Hook.LeftTransformation as LT


tests :: ReifiesNModContext ctx => Reader (Proxy ctx) TestTree
tests =
  testGroup "HookeTests" <$> sequence
  [ pure EF.properties

  , pure EFT.unitTests
  , EFT.properties

  , pure LT.unitTests
  , LT.properties
  ]
