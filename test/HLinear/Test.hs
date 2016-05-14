module Main
where

import Test.Tasty
  ( defaultMainWithIngredients
  , testGroup
  )
import Test.Tasty.Ingredients.Basic
  ( listingTests, consoleTestReporter )
import Test.Tasty.Ingredients.Rerun
  ( rerunningTests )


import HLinear.Test.PLE ( pleTests )
import HLinear.Test.Matrix ( matrixTests )


main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests [ listingTests, consoleTestReporter ] ] $
  testGroup "HLinear Tests"
    [{- debug
         matrixTests
     --, brMatrixTests
    ,-} pleTests
    ]
