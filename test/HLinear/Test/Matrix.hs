{-# LANGUAGE
    FlexibleContexts
  #-}

module HLinear.Test.Matrix
where

import qualified Data.Vector as V
import Test.Tasty ( testGroup,
                    TestTree
                  )

import qualified HLinear.Test.Matrix.Algebra as Algebra
import qualified HLinear.Test.Matrix.Basic as Basic 
import qualified HLinear.Test.Matrix.Creation as Creation
import qualified HLinear.Test.Matrix.QSTests as QSTests


matrixTests :: TestTree
matrixTests =
  testGroup "Matrix Tests"
  [ QSTests.unitTests
  , Basic.properties
  , Creation.properties
  , Algebra.properties
  ]
