module HLinear.Test.VVMatrix.QSTests
where

-- import Math.Structure
import qualified Test.SmallCheck.Series as SCS
import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ( (@?=) )
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC

import HLinear.VVMatrix


unitTests = testGroup "Quick- and SmallCheck unit tests"
  [ HU.testCase "SmallCheck" $
    ( SCS.list 1 SCS.series :: [VVMatrix Int] ) @?=
    [ zeroMatrix' Nothing Nothing
    , oneMatrix' Nothing (0)
    , zeroMatrix' (Just 0) Nothing
    , zeroMatrix 0 0
    , zeroMatrix' Nothing (Just 0)
    , oneMatrix' (Just 0) 0
    , zeroMatrix' (Just 0) (Just 0)
    , zeroMatrix 1 0
    , oneMatrix' Nothing 1
    , zeroMatrix 0 1
    , oneMatrix' (Just 0) 1
    , oneMatrix' Nothing (-1)
    , oneMatrix' (Just 0) (-1)
    ]

  , HU.testCase "QuickCheck fromLists'" $
    QC.shrink (fromLists' 1 1 [[1]] :: VVMatrix Int) @?=
      [ fromLists' 0 1 []
      , fromLists' 1 0 [[]]
      , fromLists' 1 1 [[0]]
      ]
  , HU.testCase "QuickCheck zeroMatrix'" $
    QC.shrink (zeroMatrix' (Just 2) (Just 1) :: VVMatrix Int) @?=
      [ zeroMatrix' Nothing Nothing
      , zeroMatrix' Nothing (Just 0)
      , zeroMatrix' (Just 0) Nothing
      , zeroMatrix' (Just 0) (Just 0)
      , zeroMatrix' (Just 1) Nothing
      , zeroMatrix' (Just 1) (Just 0)
      ]

  , HU.testCase "QuickCheck oneMatrix'" $
    QC.shrink (oneMatrix' (Just 5) 7 :: VVMatrix Int) @?=
      [ oneMatrix' Nothing 0
      , oneMatrix' Nothing 4
      , oneMatrix' Nothing 6
      , oneMatrix' (Just 0) 0
      , oneMatrix' (Just 0) 4
      , oneMatrix' (Just 0) 6
      , oneMatrix' (Just 3) 0
      , oneMatrix' (Just 3) 4
      , oneMatrix' (Just 3) 6
      , oneMatrix' (Just 4) 0
      , oneMatrix' (Just 4) 4
      , oneMatrix' (Just 4) 6
      ]
  ]
