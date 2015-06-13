{-# LANGUAGE
    FlexibleInstances
  #-}

module Main
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Ratio ( Ratio )
import Data.Time
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural
import System.Random

import HLinear.Matrix


main :: IO ()
main = do
  let n = 1000
  t <- getCurrentTime
  m <- randomMatrixRational n n :: IO (Matrix Rational)
  m' <- randomMatrixRational n n :: IO (Matrix Rational)
  let f = seq (m,m') $ seq t $ seq (m * m') 
  t' <- getCurrentTime
  print $ f $ diffUTCTime t' t


randomRational :: IO (Rational)
randomRational = do
  n <- randomIO :: IO Integer
  d <- let go = randomIO >>= \d' -> if d'==0 then go else return d'
       in go :: IO Integer
  return $ fromIntegral n P./ fromIntegral d

randomMatrixRational :: Natural -> Natural -> IO (Matrix Rational)
randomMatrixRational nrs ncs =
  Matrix nrs ncs <$> ( V.replicateM nrsZ $ V.replicateM ncsZ randomRational )
  where
  nrsZ = fromIntegral nrs
  ncsZ = fromIntegral ncs

randomMatrix :: Random a => Natural -> Natural -> IO (Matrix a)
randomMatrix nrs ncs = Matrix nrs ncs <$>
                         ( V.replicateM nrsZ $ V.replicateM ncsZ randomIO )
  where
  nrsZ = fromIntegral nrs
  ncsZ = fromIntegral ncs

