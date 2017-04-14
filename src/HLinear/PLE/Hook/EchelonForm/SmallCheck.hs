{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.PLE.Hook.EchelonForm.SmallCheck
where

import Control.Monad ( guard )
import qualified Data.Vector as V
import Test.SmallCheck.Series ( Serial, Series(..), series, decDepth )
import Math.Structure ( isZero, nonZero, DecidableZero )
import Numeric.Natural ()

import Test.Natural ()

import HLinear.PLE.Hook.EchelonForm.Basic
import HLinear.PLE.Hook.EchelonForm.Definition
import HLinear.PLE.Hook.EchelonForm.Row


instance    (Monad m, Serial m a, DecidableZero a)
         => Serial m (EchelonForm a)
  where
  series = do
    lrs <- series
    nrs <- series
    guard $ lrs <= nrs
    ncs <- series
  
    EchelonForm nrs ncs <$>
      ( V.replicateM (fromIntegral lrs) $ decDepth $ do
          o <- series
          guard $ o <= ncs
          EchelonFormRow o <$>
            ( V.replicateM (fromIntegral ncs - fromIntegral o) $
              decDepth $ decDepth series
            )
      )
