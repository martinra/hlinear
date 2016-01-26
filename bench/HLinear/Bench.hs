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
--import HLinear.PLE.Hook.LeftTransformation as LT
--import HLinear.PLE.Hook.RPermute as RP
import HLinear.PLE


main :: IO ()
main = defaultMain
  [
--    env ( toFMPQMat <$> uQQMatrix ) $ \mat ->
--    bgroup "FMPQMat" (bgroupFlint mat)

--    env ( toFMPQMat <$> uQQbdMatrix ) $ \mat ->
--    bgroup "FMPQMat (bounded denominator)" (bgroupFlint mat)

    env ( toFMPQMat <$> incQQMatrix ) $ \mat ->
    bgroup "FMPQMat (increasing fractions)" (bgroupFlint mat)

--  , env ( fmap fromRational <$> uQQMatrix :: IO (Matrix FMPQ) ) $ \mat ->
--    bgroup "Matrix FMPQ" (bgroupHlinearFMPQ mat)

--  , env ( fmap fromRational <$> uQQbdMatrix :: IO (Matrix FMPQ) ) $ \mat ->
--    bgroup "Matrix FMPQ (bounded denominator)" (bgroupHlinearFMPQ mat)

  , env ( fmap fromRational <$> incQQMatrix :: IO (Matrix FMPQ) ) $ \mat ->
    bgroup "Matrix FMPQ (increasing fractions)" (bgroupHlinearFMPQ mat)
--
--  , env ( uQQMatrix :: IO (Matrix Rational) ) $ \mat ->
--    bgroup "Matrix Rational" (bgroupHlinear mat)

--  , env ( uQQbdMatrix :: IO (Matrix Rational) ) $ \mat ->
--    bgroup "Matrix Rational (bounded denominator)" (bgroupHlinear mat)

--  , withNModContext 7 $ \(proxy :: Proxy ctxProxy) ->
--    env ( uFpMatrix proxy ) $ \mat ->
--    bgroup "Matrix Fp(7)" (bgroupHlinear mat)
  ]

  where
    matSize = 200
    snum = 10
    nden = 5 
    sden = 4 

--    uQQMatrix = uniformRandomMatrixQQbd matSize matSize snum nden sden
    -- uQQbdMatrix = uniformRandomMatrixQQbdLE matSize matSize snum nden sden

    incQQMatrix = increasingFractionsMatrix matSize matSize
--    uFpMatrix :: Proxy ctxProxy -> IO ( Matrix (NMod ctxProxy) )
--    uFpMatrix _ = uniformRandomMatrixFp matSize matSize 

    bgroupFlint :: FMPQMat -> [Benchmark]
    bgroupFlint mat =
      [ bench "rref" $ nf rref mat
      ]

    bgroupHlinearFMPQ mat =
      [
--        bench "ple matrices"   $ nf pleMatricesFMPQ mat
--      , bench "echelon matrix" $ nf pleEchelonMatrixFMPQ mat
        bench "rref matrix"    $ nf pleReducedEchelonMatrixFMPQ mat
      , bench "rrefff matrix"    $ nf pleReducedEchelonFFMatrixFMPQ mat
      ]

--    pleMatricesFMPQ = Hk.toMatrices . unPLEDecomposition . pleDecomposition
--    pleEchelonMatrixFMPQ = sel3 . pleMatricesFMPQ
    pleReducedEchelonMatrixFMPQ =
         EF.toMatrix . snd . REF.reducedEchelonForm
       . Hk.echelonForm . unPLEDecomposition . pleDecompositionFoldUnfold

    pleReducedEchelonFFMatrixFMPQ =
         EF.toMatrix . snd . REF.reducedEchelonForm
       . Hk.echelonForm . unPLEDecomposition . pleDecompositionFoldUnfoldFractionFree

--     bgroupHlinear mat =
--       [
-- --        bench "ple matrices"   $ nf pleMatrices mat
-- --      , bench "echelon matrix" $ nf pleEchelonMatrix mat
--         bench "rref matrix"    $ nf pleReducedEchelonMatrix mat
--       ]
-- 
--     pleMatrices = Hk.toMatrices . unPLEDecomposition . pleDecomposition
--     pleEchelonMatrix = sel3 . pleMatrices
--     pleReducedEchelonMatrix =
--          EF.toMatrix . snd . REF.reducedEchelonForm
--        . Hk._echelon . unPLEDecomposition . pleDecomposition
