module HLinear.Test.BRMatrix.QSTests
where

import Data.Either ( rights )
import qualified Test.SmallCheck.Series as SCS
import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ( (@?=) )
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC

import HLinear.BRMatrix


unitTests = testGroup "Quick- and SmallCheck unit tests"
  [ HU.testCase "SmallCheck" $
    ( SCS.list 1 SCS.series :: [BRMatrix Int] ) @?=
      rights
      [ fromLists' 0 0 []
      , fromLists' 1 0 [[]]
      , fromLists' 0 1 []
      ]

  , HU.testCase "QuickCheck fromLists'" $
    QC.shrink (fromListsUnsafe' 2 2 [[1,2],[3,4]] :: BRMatrix Int) @?=
      rights
      [ fromLists' 2 1 [[1],[3]]
      , fromLists' 2 1 [[2],[4]]
      , fromLists' 1 2 [[1,2]]
      , fromLists' 1 2 [[3,4]]
      , fromLists' 2 2 [[0,2],[3,4]]
      , fromLists' 2 2 [[1,0],[3,4]]
      , fromLists' 2 2 [[1,1],[3,4]]
      , fromLists' 2 2 [[1,2],[0,4]]
      , fromLists' 2 2 [[1,2],[2,4]]
      , fromLists' 2 2 [[1,2],[3,0]]
      , fromLists' 2 2 [[1,2],[3,2]]
      , fromLists' 2 2 [[1,2],[3,3]]
      ]
  ]
