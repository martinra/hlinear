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
-- matrices with uniformly distributed entries
--------------------------------------------------------------------------------

uniformRandomMatrixQQbd
  :: Natural -> Natural -> Natural -> Natural -> Natural -> IO (Matrix Rational)
uniformRandomMatrixQQbd nrs ncs snum nden sden =
  ( rMatrixQQbd nrs ncs
      (uniformFromSize snum)
      nden (uniformPositiveFromSize sden)
  ) `runRVar` (getRandom :: IO Word64)

uniformRandomMatrixQQbdLE
  :: Natural -> Natural -> Natural -> Natural -> Natural -> IO (Matrix Rational)
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
storeMatrices path nmb mat =
  V.generateM nmb $ \mx ->
    let filepath = "." </> path </>
             "matfmpq_bdden_mx" <> show mx
          <> "_nrs" <> show nrs <> "_ncs" <> show ncs
          <> "_snum" <> show snum
          <> "_nden" <> show nden <> "_sden" <> show sden <.> "mat"
    in  mat >>= writeFile filepath . showMatrixFMPQ

--      matenv mx = uniformRandomMatrixQQbd   mx nrs ncs snum nden sden 
--      maxmx = 50
--      nrs = 100
--      ncs = 1000
--      snum = 20
--      nden = 4
--      sden = 2
