{-# LANGUAGE
    FlexibleContexts
  #-}

module VVMatrixTests
where

import qualified Data.Vector as V
import Test.Tasty ( testGroup,
                    TestTree
                  )

import HLinear.VVMatrix

import qualified TestHLinear.Utils as U
import TestHLinear.Utils ( testProperty )

import qualified VVMatrixTests.Creation as Creation
import qualified VVMatrixTests.QSTests as QSTests


vvMatrixTestGroup :: TestTree
vvMatrixTestGroup =
  testGroup "VVMatrix Tests"
  [ QSTests.unitTests
  , Creation.properties
  ]
