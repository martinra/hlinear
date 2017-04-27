{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.Hook.EchelonTransformation.SmallCheck
where

import Control.Monad ( guard )
import qualified Data.Vector as V
import Test.SmallCheck.Series ( Serial, Series(..), series )
import Math.Structure ( isZero, nonZero, DecidableZero )
import Numeric.Natural ()

import Test.Natural ()

import HLinear.Hook.EchelonTransformation.Basic
import HLinear.Hook.EchelonTransformation.Column
import HLinear.Hook.EchelonTransformation.Definition


instance    (Monad m, Serial m a, DecidableZero a)
         => Serial m (EchelonTransformation a)
  where
  series = do
    nrs <- series
    ncs <- series
    guard $ nrs >= ncs
  
    return . EchelonTransformation nrs =<<
      V.generateM (fromIntegral ncs) ( \jx ->
        return . EchelonTransformationColumn jx =<<
          V.replicateM (fromIntegral nrs - jx - 1) series
      )