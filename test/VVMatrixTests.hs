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


equals :: Eq r
       => (VVMatrix a -> r)
       -> (VVMatrix a -> r)
       -> VVMatrix a -> Bool
equals f g m = f m == g m

equalsQQ :: Eq r
       => (VVMatrix Rational -> r) -> (VVMatrix Rational -> r)
       -> VVMatrix Rational -> Bool
equalsQQ = equals


testProperty s p = testGroup "(QuickCheck & SmallCheck)"
  [ QC.testProperty s p
  , SC.testProperty s p
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "fromLists . toLists == id" $
      equalsQQ id (fromLists . toLists)
  ]

unitTests = testGroup "Unit tests" []

