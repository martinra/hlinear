module HLinear.Test.Matrix.QSTests
where

import Data.Either ( rights )
import qualified Test.SmallCheck.Series as SCS
import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ( (@?=) )
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC

import HLinear.Matrix


unitTests = testGroup "Quick- and SmallCheck unit tests"
  [ HU.testCase "SmallCheck" $
    ( SCS.list 1 SCS.series :: [Matrix Int] )
    @?=
    ( rights
      [ fromLists' 0 0 []
      , fromLists' 0 1 []
      , fromLists' 1 0 [[]]
      ]
    )

  , HU.testCase "QuickCheck fromLists'" $
    ( take 4 $ QC.shrink (fromListsUnsafe' 2 2 [[1,2],[3,4]] :: Matrix Int) )
    @?=
    ( rights
      [ fromLists' 2 1 [[1],[3]]
      , fromLists' 2 1 [[2],[4]]
      , fromLists' 1 2 [[1,2]]
      , fromLists' 1 2 [[3,4]]
      ]
    )
  ]
