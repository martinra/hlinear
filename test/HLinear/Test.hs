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
  withNModContextM 1125899906842679 $ \proxy ->
  defaultMainWithIngredients
  [ rerunningTests [ listingTests, consoleTestReporter ] ] $
  testGroup "HLinear Tests"
    [ Matrix.tests `runReader` proxy
    , Hook.tests `runReader` proxy 
    , NormalForm.tests `runReader` proxy 
    ]
