module HLinear.Matrix.SmallCheck
where

import HLinear.Utility.Prelude

import Test.SmallCheck.Series (Serial, series, decDepth )
import qualified Data.Vector as V

import HLinear.Matrix.Definition ( Matrix(..) )


instance (Monad m, Serial m a) => Serial m (Matrix a)
  where
  series = do
    nrs <- series
    guard $ nrs >= 0
    ncs <- series
    guard $ ncs >= 0
    return . Matrix nrs ncs =<< (
      V.sequence $ V.iterateN nrs decDepth $
      V.sequence $ V.iterateN ncs decDepth $
      decDepth $ decDepth series )
