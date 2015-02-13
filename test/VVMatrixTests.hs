{-# LANGUAGE
    FlexibleContexts
  #-}

module VVMatrixTests
where

import Test.Tasty ( testGroup,
                    TestTree
                  )
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import HLinear.VVMatrix


vvMatrixTestGroup :: TestTree
vvMatrixTestGroup = testGroup "VVMatrix Tests" [properties, unitTests]



unitTests = testGroup "Unit tests" []
--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT
--
--   -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]
