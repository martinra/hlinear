{-# LANGUAGE
    FlexibleContexts
  #-}

module HLinear.Test.VVMatrix
where

import qualified Data.Vector as V
import Test.Tasty ( testGroup,
                    TestTree
                  )

import qualified HLinear.Test.VVMatrix.Algebra as Algebra
import qualified HLinear.Test.VVMatrix.Basic as Basic 
import qualified HLinear.Test.VVMatrix.Creation as Creation
import qualified HLinear.Test.VVMatrix.QSTests as QSTests


vvMatrixTests :: TestTree
vvMatrixTests =
  testGroup "VVMatrix Tests"
  [ QSTests.unitTests
  , Basic.properties
  , Creation.properties
  , Algebra.properties
  ]
