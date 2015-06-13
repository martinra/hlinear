{-# LANGUAGE
    FlexibleContexts
  #-}

module HLinear.Test.BRMatrix
where

import qualified Data.Vector as V
import Test.Tasty ( testGroup,
                    TestTree
                  )

import qualified HLinear.Test.BRMatrix.Algebra as Algebra
import qualified HLinear.Test.BRMatrix.Basic as Basic 
import qualified HLinear.Test.BRMatrix.Creation as Creation
import qualified HLinear.Test.BRMatrix.QSTests as QSTests


brMatrixTests :: TestTree
brMatrixTests =
  testGroup "BRMatrix Tests"
  [ QSTests.unitTests
  , Basic.properties
  , Creation.properties
  , Algebra.properties
  ]
