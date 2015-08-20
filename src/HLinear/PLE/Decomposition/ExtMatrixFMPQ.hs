{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , StandaloneDeriving
  , TypeFamilies
  #-}

module HLinear.PLE.Decomposition.ExtMatrixFMPQ
where

import Control.DeepSeq ( NFData(..) )
import HFlint.FMPQ
import Math.Algebra.MonicExtension
import Math.Structure

import HLinear.PLE.Hook.Definition ( PLEHook(..) )
import qualified HLinear.PLE.Hook.Definition as H
import HLinear.Matrix ( Matrix )
import HLinear.Matrix.Extension ( ExtMatrix )

import HLinear.PLE.Decomposition.Definition


newtype instance PLEDecomposition (ExtMatrix a ctx) =
  PLEDecomposition
  { unPLEDecomposition :: PLEHook (Extension a ctx a)
  }

deriving instance Show a => Show (PLEDecomposition (ExtMatrix a ctx))

instance NFData a => NFData (PLEDecomposition (ExtMatrix a ctx)) where
  rnf (PLEDecomposition hook) = seq (rnf hook) ()


toMatrices
  :: ( ReifiesExtensionCtx FMPQ ctx )
  => PLEDecomposition (ExtMatrix FMPQ ctx)
  -> ( Matrix (Extension FMPQ ctx FMPQ)
     , Matrix (Extension FMPQ ctx FMPQ)
     , Matrix (Extension FMPQ ctx FMPQ) )
toMatrices = H.toMatrices . unPLEDecomposition
