{-# LANGUAGE
    FlexibleInstances
  , StandaloneDeriving
  , TypeFamilies
  #-}

module HLinear.PLE.Decomposition.Matrix
where

import Control.DeepSeq ( NFData(..) )
import Math.Structure

import HLinear.PLE.Hook.Definition ( PLEHook(..) )
import qualified HLinear.PLE.Hook.Definition as H
import HLinear.Matrix ( Matrix )

import HLinear.PLE.Decomposition.Definition


newtype instance PLEDecomposition (Matrix a) =
  PLEDecomposition
  { unPLEDecomposition :: PLEHook a
  }

deriving instance Show a => Show (PLEDecomposition (Matrix a))

instance NFData a => NFData (PLEDecomposition (Matrix a)) where
  rnf (PLEDecomposition (PLEHook p l e)) =
    seq (rnf p) $ seq (rnf l) $ seq (rnf e) ()


toMatrices
  :: ( DecidableZero a, DivisionRing a )
  => PLEDecomposition (Matrix a)
  -> ( Matrix a, Matrix a, Matrix a )
toMatrices = H.toMatrices . unPLEDecomposition
