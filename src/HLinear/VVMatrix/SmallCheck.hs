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
import Test.SmallCheck.Series ( Serial, Series(..), series
                              , generate
                              , (\/) 
                              )

import HLinear.VVMatrix.Definition ( VVMatrix(..) )

instance (Monad m, Serial m a)
      => Serial m (VVMatrix a)
  where
  series = seriesZero \/ seriesOne \/ seriesVV
    where
    seriesZero = do
      nrs <- series
      ncs <- series
      if maybe False (<0) nrs || maybe False (<0) ncs
        then empty
        else return $ Zero nrs ncs
    seriesOne = do
      nrs <- series
      if maybe False (<0) nrs
        then empty
        else return . One nrs =<< (series :: Series m a)
   
    seriesZeroOne = generate $ const [Zero, One]
    seriesVV = do
      nrs <- series
      ncs <- series
      if nrs < 0 || ncs < 0 then empty
      else return . VVMatrix nrs ncs =<<
        return . V.replicate nrs . V.replicate ncs =<< series
