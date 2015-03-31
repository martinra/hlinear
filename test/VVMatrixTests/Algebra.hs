{-# LANGUAGE
    DataKinds
  #-}

module VVMatrixTests.Algebra
where

import Control.Monad.Reader ( runReader )
import Data.Proxy
import Math.Structure.Tasty
import Test.Tasty

import HLinear.VVMatrix
import TestHLinear.Utils ( testPropertyVVMatrix )


properties :: TestTree
properties = testGroup "Algebra properties" $
  (`runTestR` testPropertyVVMatrix) $
  isAbelianGroup (Proxy :: Proxy (SizedVVMatrix 2 2 Integer))
