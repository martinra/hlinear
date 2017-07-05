{-# LANGUAGE UndecidableInstances #-}

module HLinear.Hook.LeftTransformation.SmallCheck
where

import HLinear.Utility.Prelude

import qualified Data.Vector as V
import Test.SmallCheck.Series ( Serial, series, (\/) )

import Test.Natural ()

import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.Matrix.SmallCheck
import HLinear.Hook.LeftTransformation.Column
import HLinear.Hook.LeftTransformation.Definition
import qualified HLinear.Matrix.Naive as MNaive


instance ( Monad m, Ring a, DecidableUnit a, Serial m a, Serial m (Unit a) )
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
      ltMatrix = do
        m@(Matrix nrs ncs _) <- series
        guard $ nrs == ncs
        guard $ isUnit $ MNaive.det m
        return $ LeftTransformationMatrix m
