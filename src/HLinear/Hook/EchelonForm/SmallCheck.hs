module HLinear.Hook.EchelonForm.SmallCheck
where

import HLinear.Utility.Prelude

import Math.Structure ( isZero, nonZero, DecidableZero )
import Test.Natural ()
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
    nrs <- series
    guard $ lrs <= nrs
    ncs <- series
  
    EchelonForm nrs ncs <$>
      ( V.replicateM (fromIntegral lrs) $ decDepth $ do
          o <- series
          guard $ o <= ncs
          EchelonFormRow o <$>
            ( V.replicateM (fromIntegral ncs - fromIntegral o) $
              decDepth $ decDepth series
            )
      )
