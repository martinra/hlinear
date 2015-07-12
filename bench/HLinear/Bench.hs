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

import Control.Exception ( tryJust )
import Control.Monad ( guard )
import Criterion.Main
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
import HFlint.NMod

import HLinear.Bench.Random ( uniformRandomMatrixIO )
import HLinear.Matrix as M
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.PLE.Hook.RPermute as RP
import HLinear.PLE.Hook.PLE
import HLinear.PLE.PLE




pleEvalLE :: ( DecidableZero a, DivisionRing a )
          => Matrix a -> (Matrix a, Matrix a)
pleEvalLE mat =
  let pledec = ple mat
      p = fromMatrixPermute $ permutation pledec
      lm = left pledec
      em = echelon pledec
  in (lm,em)

pleEvalE :: ( DecidableZero a, DivisionRing a )
         => Matrix a -> Matrix a
pleEvalE = echelon . ple

main :: IO ()
main = defaultMain $ ($20) $ \matSize ->
  [
    env ( toFMPQMat <$> uniformMatrix 10 matSize matSize ) $ \mat ->
    bgroup "FMPQMat"
    [ bench "rref" $ nf rref mat
    ]

  , env ( M.map fromRational <$> uniformMatrix 10 matSize matSize :: IO (Matrix FMPQ) ) $ \mat ->
    bgroup "Rational"
    [ bench "ple all" $ nf pleEvalLE mat
    , bench "ple matrix only" $ nf pleEvalE mat
    ]
  ]

--  [ env ( return (exampleFMPQMat matSize :: FMPQMat) ) $ \mat ->
--    bgroup "FMPQMat"
--    [ bench "rref" $ nf rref mat
--    ]
--
--  , env ( return (exampleMatrix matSize :: Matrix Rational) ) $ \mat ->
--    bgroup "Rational"
--    [ bench "ple all" $ nf pleEvalLE mat
--    , bench "ple matrix only" $ nf pleEvalE mat
--    ]
--  , env ( return (exampleMatrix matSize :: Matrix FMPQ) ) $ \mat ->
--    bgroup "FMPQ"
--    [ bench "ple all" $ nf pleEvalLE mat
--    , bench "ple matrix only" $ nf pleEvalE mat
--    ]
--
--  , withNModContext 7 $ \(_ :: Proxy ctxProxy) -> 
--    env ( return (exampleMatrix matSize :: Matrix (NMod ctxProxy) ) ) $ \mat ->
--      bgroup "NMod"
--      [ bench "ple all" $ nf pleEvalLE mat
--      , bench "ple matrix only" $ nf pleEvalE mat
--      ]
--  ]
