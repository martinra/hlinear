{-# LANGUAGE
    FlexibleContexts
  #-}

module VVMatrixTests
where

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

import qualified TestHLinear.Utils as U
import TestHLinear.Utils ( testProperty )
import VVMatrixTests.Utils


vvMatrixTestGroup :: TestTree
vvMatrixTestGroup = testGroup "VVMatrix Tests"
                    [ properties
                    , unitTests ]


--  smallcheck-depth=2 is the only option that lets this run in reasonable time
properties :: TestTree
properties = testGroup "Properties"
  [ QC.testProperty "fromVectors' . toVectors == id" $ \m ->
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
