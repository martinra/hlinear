module HLinear.Test.NormalForm
where

import HLinear.Utility.Prelude

import Test.Tasty

import qualified HLinear.Test.NormalForm.FoldUnfold.PLE as PLE
import qualified HLinear.Test.NormalForm.FoldUnfold.RREF as RREF


tests :: ReifiesNModContext ctx => Reader (Proxy ctx) TestTree
tests =
  testGroup "Normal Form Tests" <$> sequence
  [ PLE.properties
  , RREF.properties
  ]
