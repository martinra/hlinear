module HLinear.Test.Matrix
where

import HLinear.Utility.Prelude

import qualified Data.Vector as V
import Test.Tasty

import qualified HLinear.Test.Matrix.Algebra as Algebra 
import qualified HLinear.Test.Matrix.Basic as Basic 
import qualified HLinear.Test.Matrix.QSTests as QSTests


tests :: ReifiesNModContext ctx => Reader (Proxy ctx) TestTree
tests =
  testGroup "Matrix tests" <$> sequence
  [ pure QSTests.unitTests
  , Algebra.properties
  , pure Basic.properties
  ]
