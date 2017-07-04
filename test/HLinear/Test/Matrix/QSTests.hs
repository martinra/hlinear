module HLinear.Test.Matrix.QSTests
where

import HLinear.Utility.Prelude
import qualified Prelude as P

import Test.Tasty
import Test.Tasty.HUnit ( testCase, (@?=) )
import qualified Test.SmallCheck.Series as SCS
import qualified Test.QuickCheck as QC

import HLinear.Matrix


unitTests = testGroup "Quick- and SmallCheck unit tests"
  [ testCase "SmallCheck" $
    ( SCS.list 1 SCS.series :: [Matrix Int] )
    @?=
    ( [ fromLists' 0 0 []
      , fromLists' 0 1 []
      , fromLists' 1 0 [[]]
      ]
    )

  , testCase "QuickCheck fromLists'" $
    ( P.take 4 $ QC.shrink (fromLists' 2 2 [[1,2],[3,4]] :: Matrix Int) )
    @?=
    ( [ fromLists' 2 1 [[1],[3]]
      , fromLists' 2 1 [[2],[4]]
      , fromLists' 1 2 [[1,2]]
      , fromLists' 1 2 [[3,4]]
      ]
    )
  ]
