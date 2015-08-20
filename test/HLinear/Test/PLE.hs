module HLinear.Test.PLE
where

import Test.Tasty

import qualified HLinear.Test.PLE.FoldUnfold.Echelonize as FUE
import qualified HLinear.Test.PLE.FoldUnfold.ReducedEchelonForm as FURE
import qualified HLinear.Test.PLE.Hook.EchelonForm as EF
import qualified HLinear.Test.PLE.Hook.EchelonTransformation as EFT
import qualified HLinear.Test.PLE.Hook.LeftTransformation as LT
import qualified HLinear.Test.PLE.Sliced.Positions as SP
import qualified HLinear.Test.PLE.Sliced.Echelonize as SE
import qualified HLinear.Test.PLE.MultiMod.Echelonize as MME


pleTests :: TestTree
pleTests =
  testGroup "PLE Tests"
  [ EF.echelonFormProperties
  --, EFT.echelonTransformationTests
  --, LT.leftTransformationTests

  --, FUE.pleProperties
  --, FURE.reducedEchelonFormProperties

  -- , MME.pleProperties

  , SP.positionProperties
  , SE.pleProperties
  ]
