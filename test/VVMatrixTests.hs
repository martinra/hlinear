{-# LANGUAGE
    FlexibleContexts
  #-}

module VVMatrixTests
where

import qualified Data.Vector as V
import Test.Tasty ( testGroup,
                    TestTree
                  )

import qualified VVMatrixTests.Algebra as Algebra
import qualified VVMatrixTests.Basic as Basic 
import qualified VVMatrixTests.Creation as Creation
import qualified VVMatrixTests.QSTests as QSTests


vvMatrixTests :: TestTree
vvMatrixTests =
  testGroup "VVMatrix Tests"
  [ QSTests.unitTests
  , Basic.properties
  , Creation.properties
  , Algebra.properties
  ]
