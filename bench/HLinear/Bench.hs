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
import Control.Exception ( tryJust )
import Control.Monad ( guard )
import Criterion.Main
import Criterion.Types ( Config(..), Verbosity(..) )
import qualified Data.Binary as B
import Data.Maybe
import Data.Proxy
import qualified Data.Vector as V
import Data.Tuple.Select ( sel3 )
import Math.Structure
import Numeric.Natural
import System.FilePath
import System.IO.Error ( isDoesNotExistError )

import HFlint.FMPQ
import qualified HFlint.FMPQMat as FMPQMat
import HFlint.NMod

import HLinear.Bench.Example
  ( increasingFractionsMatrix
  , uniformRandomMatrixQQbd
  , uniformRandomMatrixQQbdLE
  )
import HLinear.Bench.Conversion ( toFMPQMat )
import HLinear.Matrix as M
import HLinear.PLE.Hook as Hk
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.ReducedEchelonForm as REF
import HLinear.PLE

main :: IO ()
main = defaultMainWith
  defaultConfig { csvFile = Just "benchmarks.csv" } $
  [ env (matenv mx) $ \mat ->
      bench ("rref" ++ show mx) $ nf rref mat
  | mx <- [1..maxmx]
  ] ++
  [ env (toFMPQMat <$> matenv mx) $ \mat ->
      bench ("FLINTrref" ++ show mx) $ nf rref mat
  | mx <- [1..maxmx]
  ]
  where
    matenv mx = uniformRandomMatrixQQbd   mx nrs ncs snum nden sden 
    maxmx = 50
    nrs = 100
    ncs = 1000
    snum = 20
    nden = 4
    sden = 2
