{-# LANGUAGE
    FlexibleInstances
  , ScopedTypeVariables
  #-}

module Main
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.DeepSeq ( NFData )
-- import Control.Exception ( tryJust )
import Control.Exception
-- import Control.Monad ( guard )
import Control.Monad
import qualified Data.Binary as B
import Data.Maybe
import Data.Proxy
import qualified Data.Vector as V
import Data.Tuple.Select ( sel3 )
import Math.Structure
import Numeric.Natural
import System.FilePath
import System.IO.Error ( isDoesNotExistError )

import Formatting
import Formatting.Clock
import System.Clock
import Test.QuickCheck

import HFlint.FMPQ
import HFlint.FMPQMat as FMPQMat
import HFlint.NMod

import HLinear.Bench.Example
  ( increasingFractionsMatrix
  , uniformRandomMatrixQQbd
  , uniformRandomMatrixQQbdLE
  , uniformRandomMatrixFp
  )
import HLinear.Bench.Conversion ( toFMPQMat )
import HLinear.Matrix as M
import HLinear.PLE.Hook as Hk
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.ReducedEchelonForm as REF
import HLinear.PLE


main :: IO ()
main = replicateM_ 100 $ do
  let matSize = 39
      snum = 10
      nden = 5
      sden = 4
      pleReducedEchelonMatrixFMPQ =
           EF.toMatrix . snd . REF.reducedEchelonForm
         . Hk.echelonForm . unPLEDecomposition . pleDecomposition
  uQQbdMatrix <- uniformRandomMatrixQQbdLE matSize matSize snum nden sden
  let flintMat = toFMPQMat uQQbdMatrix
      hlinearMat = fmap fromRational uQQbdMatrix :: Matrix FMPQ
  start <- getTime Monotonic
  evaluate (rref flintMat)
  end <- getTime Monotonic
  fprint timeSpecs start end
  putStr ",  "
  start <- getTime Monotonic
  evaluate (pleReducedEchelonMatrixFMPQ hlinearMat)
  end <- getTime Monotonic
  fprint timeSpecs start end
  putStrLn ""
