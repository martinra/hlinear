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
  series = seriesZeroOne \/ seriesVV
    where
    seriesZeroOne = generate $ const [Zero, One]
    seriesVV = do
      nrs <- series :: Series m Int
      ncs <- series :: Series m Int
      if nrs < 0 || ncs < 0 then empty
      else do
        rs <- V.replicateM nrs $ V.replicateM ncs (series :: Series m a)
        return $ VVMatrix nrs ncs rs
