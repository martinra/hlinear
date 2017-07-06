module HLinear.Hook.EchelonTransformation.SmallCheck
where

import HLinear.Utility.Prelude
import qualified Prelude as P

import Test.SmallCheck.Series ( Serial, Series(..), series )
import qualified Data.Vector as V

import HLinear.Hook.EchelonTransformation.Basic
import HLinear.Hook.EchelonTransformation.Column
import HLinear.Hook.EchelonTransformation.Definition


instance    (Monad m, Serial m a, DecidableZero a)
         => Serial m (EchelonTransformation a)
  where
  series = do
    nrs <- series
    ncs <- series
    guard $ ncs >= 0 && nrs >= ncs
  
    return . EchelonTransformation nrs =<<
      V.generateM ncs ( \jx ->
        return . EchelonTransformationColumn jx =<<
          V.replicateM (nrs-jx-1) series
      )
