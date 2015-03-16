{-# LANGUAGE
    FlexibleContexts
  #-}

module VVMatrixTests
where

import Debug.Trace ( trace )
import qualified Data.Vector as V
import Test.Tasty ( testGroup,
                    TestTree
                  )
import qualified Test.SmallCheck.Series as SCS
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ( (@?=) )

import HLinear.VVMatrix


vvMatrixTestGroup :: TestTree
vvMatrixTestGroup = testGroup "VVMatrix Tests"
                    [ properties, unitTests ]


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
--  smallcheck-depth=2 is the only option that lets this run in reasonable time
--  , SC.testProperty s p
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "fromVectors' . toVectors == id" $ \m ->
      trace ("testing " ++ show (nrows m) ++ ", " ++ show (ncols m)) $
      (m :: VVMatrix Int)
         == (let nrs = nrows m
                 ncs = ncols m
                 rs  = toVectors m
             in fromVectors' nrs ncs rs)
  ]

unitTests = testGroup "Unit tests"
  [ HU.testCase "SmallCheck" $
    ( SCS.list 1 SCS.series :: [VVMatrix Int] ) @?=
      [ fromLists' 0 0 []
      , fromLists' 0 1 []
      , fromLists' 1 0 [[]]
      , fromLists' 1 1 [[0]]
      , fromLists' 1 1 [[1]]
      , fromLists' 1 1 [[-1]]
      ]

  , HU.testCase "QuickCheck" $
    QC.shrink (fromLists' 1 1 [[1]] :: VVMatrix Int) @?=
      [ fromLists' 0 1 []
      , fromLists' 1 0 [[]]
      , fromLists' 1 1 [[0]]
      ]
  ]
