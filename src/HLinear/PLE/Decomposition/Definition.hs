{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module HLinear.PLE.Decomposition.Definition
where

import Control.DeepSeq ( NFData(..) )
import Math.Structure

import HLinear.PLE.Hook ( PLEHook(..) )
import qualified HLinear.PLE.Hook as H
import HLinear.Matrix ( Matrix )


newtype PLEDecomposition a =
  PLEDecomposition
  { unPLEDecomposition :: PLEHook a
  }
  deriving Show

class HasPLEDecomposition f a where
  pleDecomposition :: f a -> PLEDecomposition a


instance NFData a => NFData (PLEDecomposition a) where
  rnf (PLEDecomposition (PLEHook p l e)) =
    seq (rnf p) $ seq (rnf l) $ seq (rnf e) ()


toMatrices
  :: ( DecidableZero a, DivisionRing a )
  => PLEDecomposition a
  -> ( Matrix a, Matrix a, Matrix a )
toMatrices = H.toMatrices . unPLEDecomposition
