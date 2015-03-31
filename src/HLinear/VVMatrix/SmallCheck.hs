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
import Data.Proxy ( Proxy(..) )
import qualified Data.Vector as V
import GHC.TypeLits ( Nat, KnownNat, natVal )
import Numeric.Natural ()

import Test.SmallCheck.Series ( Serial, Series(..), series
                              , decDepth
                              , (\/) 
                              )
import Test.Natural ()

import HLinear.VVMatrix.Definition ( VVMatrix(..), SizedVVMatrix(..) )


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
      return . VVMatrix nrs ncs =<< (
        V.sequence $ V.iterateN (fromIntegral nrs) decDepth $
        V.sequence $ V.iterateN (fromIntegral ncs) decDepth $
        decDepth $ decDepth series )

instance (KnownNat nrs, KnownNat ncs, Monad m, Serial m a)
      => Serial m (SizedVVMatrix nrs ncs a)
  where
  series = return . SizedVVMatrix .
           VVMatrix (fromInteger nrs) (fromInteger ncs) =<<
             ( V.replicateM nrs' $ V.replicateM ncs' series )
    where
    nrs = natVal ( Proxy :: Proxy nrs )
    ncs = natVal ( Proxy :: Proxy ncs )
    nrs' = fromInteger nrs :: Int
    ncs' = fromInteger ncs :: Int
