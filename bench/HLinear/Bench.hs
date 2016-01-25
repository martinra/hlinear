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
main = defaultMain
  [
--    env ( toFMPQMat <$> uQQMatrix ) $ \mat ->
--    bgroup "FMPQMat" (bgroupFlint mat)

    env ( toFMPQMat <$> uQQbdMatrix ) $ \mat ->
    bgroup "FMPQMat bounded denominator" (bgroupFlint mat)

--  , env ( fmap fromRational <$> uQQMatrix :: IO (Matrix FMPQ) ) $ \mat ->
--    bgroup "Matrix FMPQ" (bgroupHlinearFMPQ mat)

  , env ( fmap fromRational <$> uQQbdMatrix :: IO (Matrix FMPQ) ) $ \mat ->
    bgroup "Matrix FMPQ (bounded denominator)" (bgroupHlinearFMPQ mat)

--  , env ( uQQMatrix :: IO (Matrix Rational) ) $ \mat ->
--    bgroup "Matrix Rational" (bgroupHlinear mat)

--  , env ( uQQbdMatrix :: IO (Matrix Rational) ) $ \mat ->
--    bgroup "Matrix Rational (bounded denominator)" (bgroupHlinear mat)

--  , withNModContext 7 $ \(proxy :: Proxy ctxProxy) ->
--    env ( uFpMatrix proxy ) $ \mat ->
--    bgroup "Matrix Fp(7)" (bgroupHlinear mat)
  ]

  where
    matSize = 50
    snum = 10
    nden = 5
    sden = 4

    uQQbdMatrix = uniformRandomMatrixQQbdLE matSize matSize snum nden sden

    bgroupFlint :: FMPQMat -> [Benchmark]
    bgroupFlint mat =
      [ bench "rref" $ nf rref mat
      ]

    bgroupHlinearFMPQ mat =
      [
        bench "rref matrix"    $ nf pleReducedEchelonMatrixFMPQ mat
      ]

    pleReducedEchelonMatrixFMPQ =
         EF.toMatrix . snd . REF.reducedEchelonForm
       . Hk.echelonForm . unPLEDecomposition . pleDecomposition
