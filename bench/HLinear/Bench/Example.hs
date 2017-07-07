{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module HLinear.Bench.Example
where

import HLinear.Utility.Prelude

import Control.Exception ( tryJust )
import Control.Monad ( guard )
import Control.Monad.Random ( getRandom )
import Data.Bits ( shiftL )
import Data.Random ( runRVar, RVar, Uniform(..), StdUniform(..) )
import Data.Ratio ( Rational )
import Data.Word ( Word64 )
import Numeric.Natural ( Natural )
import System.IO ( IO, writeFile )
import System.FilePath ( FilePath, (<.>), (</>) )

import HLinear.Bench.Random
  ( rMatrix, rMatrixQQbd, rMatrixQQbdLE
  )
import HLinear.Bench.MatrixParser ( showMatrixRational )
import HLinear.NormalForm.PLE as PLE
import HLinear.Matrix as M


--------------------------------------------------------------------------------
-- matrices with uniformly distributed entries
--------------------------------------------------------------------------------

uniformRandomMatrixQQbd
  :: Int -> Int -> Natural -> Natural -> Natural -> IO (Matrix Rational)
uniformRandomMatrixQQbd nrs ncs snum nden sden =
  ( rMatrixQQbd nrs ncs
      (uniformFromSize snum)
      nden (uniformPositiveFromSize sden)
  ) `runRVar` (getRandom :: IO Word64)

uniformRandomMatrixQQbdLE
  :: Int -> Int -> Natural -> Natural -> Natural -> IO (Matrix Rational)
uniformRandomMatrixQQbdLE nrs ncs snum nden sden =
  ( rMatrixQQbdLE nrs ncs
      (uniformFromSize snum)
      nden (uniformPositiveFromSize sden)
  ) `runRVar` (getRandom :: IO Word64)

--------------------------------------------------------------------------------
-- utility
--------------------------------------------------------------------------------

uniformFromSize :: Natural -> Uniform Integer
uniformFromSize s = Uniform (-u) u
    where
      u = fromInteger $ shiftL 1 (8 * fromIntegral s)

uniformPositiveFromSize :: Natural -> Uniform Natural
uniformPositiveFromSize s = Uniform 1 $ shiftL 1 (8 * fromIntegral s)
    
storeMatrices
  :: FilePath -> Int -> IO (Matrix Rational) -> IO ()
storeMatrices filepath nmb mat =
  forM_ [0..nmb-1] $ \mx ->
    mat >>= writeFile (filepath <> show mx) . showMatrixRational

--      matenv mx = uniformRandomMatrixQQbd   mx nrs ncs snum nden sden 
--      maxmx = 50
--      nrs = 100
--      ncs = 1000
--      snum = 20
--      nden = 4
--      sden = 2
