module HLinear.Test.PLE
where

import Test.Tasty

import qualified HLinear.Test.NormalForm.PLE.DivisionRing as PLEDR
import qualified HLinear.Test.NormalForm.RREF.DivisionRing as RREFDR


tests :: TestTree
tests =
  testGroup "Normal Form Tests"
  [ PLEDR.properties
  , RREFDR.properties
  ]
