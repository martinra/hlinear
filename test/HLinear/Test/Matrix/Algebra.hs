{-# LANGUAGE
    DataKinds
  #-}

module HLinear.Test.Matrix.Algebra
where

import Control.Monad.Reader ( runReader )
import Data.Proxy
import Math.Structure.Tasty
import Test.Tasty

import HLinear.Matrix
import HLinear.Matrix.Maybe
import HLinear.Test.Utils ( testPropertyMatrix )

properties :: TestTree
properties = testGroup "Algebra properties" $
  (`runTestR` testPropertyMatrix) $
  fmap concat $ sequence
  [ isSemiring (Proxy :: Proxy (MaybeMatrix Integer))
 ]
