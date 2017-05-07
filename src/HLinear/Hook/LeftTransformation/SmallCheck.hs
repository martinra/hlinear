{-# LANGUAGE UndecidableInstances #-}

module HLinear.Hook.LeftTransformation.SmallCheck
where

import HLinear.Utility.Prelude

import qualified Data.Vector as V
import Test.SmallCheck.Series ( Serial, series, (\/) )
import Numeric.Natural ()

import Test.Natural ()

import HLinear.Matrix.SmallCheck
import HLinear.Hook.LeftTransformation.Column
import HLinear.Hook.LeftTransformation.Definition


instance ( Monad m, Ring a, Serial m a, Serial m (Unit a) )
  => Serial m (LeftTransformation a)
  where
  series = ltColumn \/ ltMatrix
    where
      ltColumn = do
        nrs <- series
        ncs <- series
        guard $ nrs >= ncs
      
        cs <- V.generateM (fromIntegral ncs) ( \jx -> do
          a <- series
          bs <- V.replicateM (fromIntegral nrs - jx - 1) series
          return $ LeftTransformationColumn jx a bs
          )
        return $ LeftTransformation nrs cs
      ltMatrix = LeftTransformationMatrix <$> series
