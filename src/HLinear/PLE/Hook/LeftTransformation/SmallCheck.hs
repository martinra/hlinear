{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.PLE.Hook.LeftTransformation.SmallCheck
where

import Control.Monad ( guard )
import qualified Data.Vector as V
import Test.SmallCheck.Series ( Serial, Series(..), series )
import Math.Structure ( isZero, nonZero, DecidableZero )
import Numeric.Natural ()

import Test.Natural ()

import HLinear.PLE.Hook.LeftTransformation.Basic
import HLinear.PLE.Hook.LeftTransformation.Column


instance    (Monad m, Serial m a, DecidableZero a)
         => Serial m (LeftTransformation a)
  where
  series = do
    nrs <- series
    ncs <- series
    guard $ nrs >= ncs
  
    return . LeftTransformation nrs =<< (
      V.generateM (fromIntegral ncs) $ \jx -> do
        a <- series
        guard $ not $ isZero a
        return . LeftTransformationColumn jx (nonZero a) =<<
          V.replicateM (fromIntegral nrs - jx - 1) series
      )
