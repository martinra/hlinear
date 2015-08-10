{-# LANGUAGE
    DataKinds
  #-}

module HLinear.Test.BRMatrix.Algebra
where

import Control.Monad.Reader ( runReader )
import Data.Proxy
import Math.Structure.Tasty
import Test.Tasty

import HLinear.BRMatrix
import HLinear.Test.Utils ( testPropertyMatrix )


properties :: TestTree
properties = testGroup "Algebra properties" $
  (`runTestR` testPropertyMatrix) $
  fmap concat $ sequence
  [ isRng (Proxy :: Proxy (BRMatrix Integer))
  , isNonUnitalAlgebra (Proxy :: Proxy Integer)
                       (Proxy :: Proxy (BRMatrix Integer))
 ]
                
