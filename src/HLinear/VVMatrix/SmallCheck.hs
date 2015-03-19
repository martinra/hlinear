{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  #-}

module HLinear.VVMatrix.SmallCheck
where

import Control.Applicative ( empty )
import Control.Monad ( when )
import qualified Data.Vector as V
import Numeric.Natural ()

import Test.SmallCheck.Series ( Serial, Series(..), series
                              , generate
                              , (\/) 
                              )
import Test.Natural ()

import HLinear.VVMatrix.Definition ( VVMatrix(..) )


instance (Monad m, Serial m a)
      => Serial m (VVMatrix a)
  where
  series = seriesZero \/ seriesOne \/ seriesVV
    where
    seriesZero = do
      nrs <- series
      ncs <- series
      return $ Zero nrs ncs

    seriesOne = do
      nrs <- series
      return . One nrs =<< (series :: Series m a)
   
    seriesVV = do
      nrs <- series
      ncs <- series
      return . VVMatrix nrs ncs =<<
        return . V.replicate (fromIntegral nrs)
               . V.replicate (fromIntegral ncs) =<<
          series
