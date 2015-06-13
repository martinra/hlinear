{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.Matrix.SmallCheck
where

import qualified Data.Vector as V

import Test.SmallCheck.Series ( Serial, series
                              , decDepth
                              )
import Test.Natural ()

import HLinear.Matrix.Definition ( Matrix(..) )


instance (Monad m, Serial m a) => Serial m (Matrix a)
  where
  series = do
    nrs <- series
    ncs <- series
    return . Matrix nrs ncs =<< (
      V.sequence $ V.iterateN (fromIntegral nrs) decDepth $
      V.sequence $ V.iterateN (fromIntegral ncs) decDepth $
      decDepth $ decDepth series )
