{-# LANGUAGE
    GADTs
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeFamilies
  #-}

module HLinear.PLE.Decomposition
where

import Math.Structure

import HLinear.PLE.Hook ( PLEHook )
import qualified HLinear.PLE.Hook as H
import HLinear.Matrix ( Matrix )


newtype PLEDecomposition a =
  PLEDecomposition
  { unPLEDecomposition :: PLEHook a
  }
  deriving Show

class HasPLEDecomposition f a where
  pleDecomposition :: f a -> PLEDecomposition a


toMatrices
  :: ( DecidableZero a, DivisionRing a )
  => PLEDecomposition a
  -> ( Matrix a, Matrix a, Matrix a )
toMatrices = H.toMatrices . unPLEDecomposition
