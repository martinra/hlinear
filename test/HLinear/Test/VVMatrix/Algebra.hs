{-# LANGUAGE
    DataKinds
  #-}

module HLinear.Test.VVMatrix.Algebra
where

import Control.Monad.Reader ( runReader )
import Data.Proxy
import Math.Structure.Tasty
import Test.Tasty

import HLinear.VVMatrix
import HLinear.Test.Utils ( testPropertyVVMatrix )


properties :: TestTree
properties = testGroup "Algebra properties" $
  (`runTestR` testPropertyVVMatrix) $
  fmap concat $ sequence
  [ isRing (Proxy :: Proxy (SizedVVMatrix 2 2 Integer))
  , isRing (Proxy :: Proxy (SizedVVMatrix 3 3 Integer))

  , isAlgebra (Proxy :: Proxy Integer)
              (Proxy :: Proxy (SizedVVMatrix 2 2 Integer))

  , isLeftModule (Proxy :: Proxy (SizedVVMatrix 2 2 Integer))
                 (Proxy :: Proxy (SizedVVMatrix 2 3 Integer))
  , isRightModule (Proxy :: Proxy (SizedVVMatrix 3 3 Integer))
                  (Proxy :: Proxy (SizedVVMatrix 2 3 Integer))
 ]
                
