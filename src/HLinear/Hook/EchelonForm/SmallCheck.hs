module HLinear.Hook.EchelonForm.SmallCheck
where

import HLinear.Utility.Prelude

import Math.Structure ( isZero, nonZero, DecidableZero )
import Test.SmallCheck.Series ( Serial, Series(..), series, decDepth )
import qualified Data.Vector as V

import HLinear.Hook.EchelonForm.Basic
import HLinear.Hook.EchelonForm.Definition
import HLinear.Hook.EchelonForm.Row


instance    (Monad m, Serial m a, DecidableZero a)
         => Serial m (EchelonForm a)
  where
  series = do
    lrs <- series
    guard $ lrs >= 0
    nrs <- series
    guard $ nrs >= lrs
    ncs <- series
  
    EchelonForm nrs ncs <$>
      ( V.replicateM lrs $ decDepth $ do
          o <- series
          guard $ 0 >= 0 && ncs >= o
          EchelonFormRow o <$>
            ( V.replicateM (ncs-o) $
              decDepth $ decDepth series
            )
      )
