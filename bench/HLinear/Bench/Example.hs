{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module HLinear.Bench.Example
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Exception ( tryJust )
import Control.Monad ( guard )
import Control.Monad.Random ( getRandom )
import qualified Data.Binary as B
import Data.Binary ( Binary )
import Data.Bits ( shiftL )
import Data.Maybe
import Data.Proxy
import Data.Random ( runRVar, RVar, Uniform(..), StdUniform(..) )
import qualified Data.Vector as V
import Data.Word ( Word64 )
import Math.Structure
import Numeric.Natural
import System.FilePath
import System.IO.Error ( isDoesNotExistError )

import HFlint.FMPQ
import HFlint.NMod ( ReifiesNModContext, NMod(..), Modulus(..), modulus )

import HLinear.Bench.Random
  ( rMatrix, rMatrixQQbd, rMatrixQQbdLE
  )
import HLinear.PLE as PLE
import HLinear.Matrix as M

--------------------------------------------------------------------------------
-- special matrices
--------------------------------------------------------------------------------

increasingFractionsMatrix
  :: ( Binary a, Fractional a, Ring a )
  => Natural -> Natural -> IO (Matrix a)
increasingFractionsMatrix nrs ncs = do
  B.encodeFile fileName m
  eitherMat <- tryJust ( guard . isDoesNotExistError ) $ B.decodeFile fileName
  case eitherMat of
    Left _   -> return m
    Right m' -> return m'

  where
    fileName = "." </> "bench_data" </> "mat_inc23_nrs" ++ show nrs ++ "_ncs" ++ show ncs <.> "mat"

    nrsZ = fromIntegral nrs
    ncsZ = fromIntegral ncs
    m = M.fromVectorsUnsafe' nrs ncs $
      V.generate nrsZ $ \ix ->
      V.generate ncsZ $ \jx ->
        let ixQ = fromIntegral ix
            jxQ = fromIntegral jx
            ncsQ = fromIntegral ncs
        in (ixQ^2 + 2) P./ ((ncsQ-jxQ)^3 + 1)

--------------------------------------------------------------------------------
-- matrices with uniformly distributed entries
--------------------------------------------------------------------------------

uniformRandomMatrixQQbd
  :: Int -> Natural -> Natural -> Natural -> Natural -> Natural -> IO (Matrix Rational)
uniformRandomMatrixQQbd mx nrs ncs snum nden sden =
  loadOrCreate
    ( rMatrixQQbd nrs ncs
        (uniformFromSize snum)
        nden (uniformPositiveFromSize sden)
    ) $
    "." </> "bench_data" </>
       "mat_qqdb_mx" ++ show mx
    ++ "_nrs" ++ show nrs ++ "_ncs" ++ show ncs
    ++ "_snum" ++ show snum
    ++ "_nden" ++ show nden ++ "_sden" ++ show sden <.> "mat"

uniformRandomMatrixQQbdLE
  :: Int -> Natural -> Natural -> Natural -> Natural -> Natural -> IO (Matrix Rational)
uniformRandomMatrixQQbdLE mx nrs ncs snum nden sden =
  loadOrCreate
    ( rMatrixQQbdLE nrs ncs
        (uniformFromSize snum)
        nden (uniformPositiveFromSize sden)
    ) $
    "." </> "bench_data" </>
       "mat_qqdble_mx" ++ show mx
    ++ "_nrs" ++ show nrs ++ "_ncs" ++ show ncs
    ++ "_snum" ++ show snum
    ++ "_nden" ++ show nden ++ "_sden" ++ show sden <.> "mat"

--------------------------------------------------------------------------------
-- utility
--------------------------------------------------------------------------------

loadOrCreate :: Binary a => RVar a -> FilePath -> IO a
loadOrCreate a fileName = do
  eitherMat <- tryJust ( guard . isDoesNotExistError ) $ B.decodeFile fileName
  case eitherMat of
    Left _   -> do
      a' <- runRVar a (getRandom :: IO Word64)
      B.encodeFile fileName a'
      return a'
    Right a' -> return a'

uniformFromSize :: Natural -> Uniform Integer
uniformFromSize s = Uniform (-u) u
    where
      u = fromInteger $ shiftL 1 (8 * fromIntegral s)

uniformPositiveFromSize :: Natural -> Uniform Natural
uniformPositiveFromSize s = Uniform 1 $ shiftL 1 (8 * fromIntegral s)
