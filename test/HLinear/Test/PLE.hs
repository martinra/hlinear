module HLinear.Test.PLE
where

import Test.Tasty

import qualified HLinear.Test.PLE.FoldUnfold.DivisionRing as FUDR
import qualified HLinear.Test.PLE.FoldUnfold.ReducedEchelonForm as FURE
import qualified HLinear.Test.PLE.Hook.EchelonForm as EF
import qualified HLinear.Test.PLE.Hook.EchelonTransformation as EFT
import qualified HLinear.Test.PLE.Hook.LeftTransformation as LT


pleTests :: TestTree
pleTests =
  testGroup "PLE Tests"
  [ EF.echelonFormProperties
  , EFT.echelonTransformationTests
  , LT.leftTransformationTests

  , FUDR.pleProperties
  , FURE.reducedEchelonFormProperties
  ]
