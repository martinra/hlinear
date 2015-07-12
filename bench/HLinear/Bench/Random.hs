module HLinear.Bench.Random
where

import Data.Bits ( shiftL )
import qualified Data.Vector as V
import Data.Word ( Word64 )
import Numeric.Natural
import System.Random ( randomIO )

import HLinear.Matrix ( Matrix(..) )


-- uniform between 0 and 2^64n
uniformRandomNaturalIO :: Int -> IO Natural
uniformRandomNaturalIO n
  | n <= 0 = return 0
  | otherwise = V.foldl' (\n w -> n `shiftL` 64 + fromIntegral (w :: Word64)) 0 <$> V.replicateM n randomIO

uniformRandomRationalIO :: Int -> IO Rational
uniformRandomRationalIO n
  | n <= 0 = return 0
  | otherwise = do
      num <- uniformRandomNaturalIO n
      den <- let go = uniformRandomNaturalIO n >>= \den' ->
                      if den' == 0 then go else return den'
             in go
      let f = fromIntegral num / fromIntegral den :: Rational
      randomIO >>= \neg -> return $ if neg then f else -f

uniformRandomMatrixIO :: Int -> Natural -> Natural -> IO (Matrix Rational)
uniformRandomMatrixIO n nrs ncs =
  Matrix nrs ncs <$> ( V.replicateM nrsZ $ V.replicateM ncsZ $ uniformRandomRationalIO n )
  where
    nrsZ = fromIntegral nrs
    ncsZ = fromIntegral ncs
