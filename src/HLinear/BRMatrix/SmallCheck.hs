{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.BRMatrix.SmallCheck
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

import HLinear.BRMatrix.Definition ( BRMatrix(..) )
import HLinear.BRMatrix.RVector ( RVector(..) )
import qualified HLinear.BRMatrix.RVector as RV


instance (Monad m, Serial m a) => Serial m (BRMatrix a)
  where
  series = do
    nrs <- series
    ncs <- series
    return . BRMatrix nrs ncs =<< (
      fmap RVector $ V.sequence $ V.iterateN (fromIntegral nrs) decDepth $
      fmap RVector $ V.sequence $ V.iterateN (fromIntegral ncs) decDepth $
      decDepth $ decDepth series )
