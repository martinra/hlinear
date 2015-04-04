module PLETests.VVMatrixField.LeftTransformation
where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import HLinear.PLE.VVMatrixField
import HLinear.PLE.VVMatrixField.LeftTransformation
import HLinear.VVMatrix hiding ( nmbRows )


leftTransformationTests :: TestTree
leftTransformationTests =
  testGroup "LeftTransformation"
  [ QC.testProperty "toVVMatrix = apply to identity" $
      \l -> toVVMatrix (l :: LeftTransformation Rational)
            == apply l (identityMatrix (nmbRows l))
  ]
