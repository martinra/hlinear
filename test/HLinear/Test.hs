module Main
where

import HLinear.Utility.Prelude
import System.IO ( IO )

import Test.Tasty
  ( defaultMainWithIngredients
  , testGroup
  )
import Test.Tasty.Ingredients.Basic
  ( listingTests, consoleTestReporter )
import Test.Tasty.Ingredients.Rerun
  ( rerunningTests )



import qualified HLinear.Test.Hook as Hook
import qualified HLinear.Test.Matrix as Matrix
import qualified HLinear.Test.NormalForm as NormalForm


main :: IO ()
main =
  defaultMainWithIngredients
  [ rerunningTests [ listingTests, consoleTestReporter ] ] $
  testGroup "HLinear Tests"
    [ Matrix.tests
    , Hook.tests
    , NormalForm.tests
    ]
