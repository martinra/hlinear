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
    []

  , HU.testCase "QuickCheck fromLists'" $
    QC.shrink (fromListsUnsafe' 1 1 [[1]] :: BRMatrix Int) @?=
      rights
      [ fromLists' 0 1 []
      , fromLists' 1 0 [[]]
      , fromLists' 1 1 [[0]]
      ]
  ]
