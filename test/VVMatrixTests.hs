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

testProperty s p = testGroup ("s " ++ "(QuickCheck & SmallCheck)")
  [ QC.testProperty s p
  , SC.testProperty s p
  ]

f :: VVMatrix a -> Bool
f x = x == fromLists (toLists x)

properties :: TestTree
properties = testGroup "Properties"
  [
    testProperty "fromLists . toLists == id" f
  ]

unitTests = testGroup "Unit tests" []

