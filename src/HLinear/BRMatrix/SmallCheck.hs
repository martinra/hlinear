{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.BRMatrix.SmallCheck
where

import Test.SmallCheck.Series ( Serial, series )

import HLinear.BRMatrix.Definition ( BRMatrix(..) )
import HLinear.Matrix.Conversion
import HLinear.Matrix.SmallCheck


instance (Monad m, Serial m a) => Serial m (BRMatrix a)
  where
  series = toBRMatrix <$> series
