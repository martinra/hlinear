{-# LANGUAGE
    ScopedTypeVariables
  #-}

module HLinear.Bench.Examples
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Exception ( tryJust )
import Control.Monad ( guard )
import qualified Data.Binary as B
import Data.Maybe
import Data.Proxy
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural
import System.FilePath
import System.IO.Error ( isDoesNotExistError )

import HFlint.FMPQ
import HFlint.FMPQMat as FMPQMat

import HLinear.Bench.Random ( uniformRandomMatrixIO )
import HLinear.Matrix as M


increasingFractionsMatrix
  :: ( Fractional a, Ring a )
  => Natural -> Natural -> Matrix a
increasingFractionsMatrix nrs ncs = m
  where
  nrsZ = fromIntegral nrs
  ncsZ = fromIntegral ncs
  Right m = M.fromVectors' nrs ncs $
    V.generate nrsZ $ \ix ->
    V.generate ncsZ $ \jx ->
      let ixQ = fromIntegral ix
          jxQ = fromIntegral jx
          ncsQ = fromIntegral ncs
      in (ixQ^2 + 2) P./ ((ncsQ-jxQ)^3 + 1)


uniformRandomMatrix :: Int -> Natural -> Natural -> IO (Matrix Rational)
uniformRandomMatrix n nrs ncs = do
  eitherMat <- tryJust ( guard . isDoesNotExistError ) $
                 B.decodeFile fileName
  case eitherMat of
    Left _  -> do
      m <- uniformRandomMatrixIO n nrs ncs
      B.encodeFile fileName m
      return m
    Right m -> return m
  where
    fileName = "." </> "bench_data" </> "u" ++ show n ++ "_nrs" ++ show nrs ++ "_ncs" ++ show ncs <.> "mat"


toFMPQMat :: Matrix Rational -> FMPQMat
toFMPQMat = FMPQMat.fromVectors . V.map (V.map fromRational) . M.rows 

