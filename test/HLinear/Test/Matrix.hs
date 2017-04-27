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
import qualified HLinear.Test.Matrix.QSTests as QSTests


tests :: TestTree
tests =
  testGroup "Matrix Tests"
  [ QSTests.unitTests
  , Algebra.properties
  , Basic.properties
  ]
