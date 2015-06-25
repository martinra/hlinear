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

import Criterion.Main
import Data.Maybe
import Data.Proxy
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural

import HFlint.FMPQ
import HFlint.NMod

import HLinear.Matrix as M
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.PLE.Hook.RPermute as RP
import HLinear.PLE.Hook.PLE
import HLinear.PLE.PLE



exampleMatrix :: ( Fractional a, Ring a )
              => Natural -> Matrix a
exampleMatrix n = m
  where
  nZ = fromIntegral n
  Right m = M.fromVectors' n n $
    V.generate nZ $ \ix ->
    V.generate nZ $ \jx ->
      let ix' = fromIntegral ix
          jx' = fromIntegral jx
          n'  = fromIntegral n
      in (ix'^2 + 2) P./ ((n'-jx')^3 + 1)

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
main = defaultMain $ ($200) $ \matSize ->
  [ env ( return (exampleMatrix matSize :: Matrix Rational) ) $ \mat ->
    bgroup "Rational"
    [ bench "ple all" $ nf pleEvalLE mat
    , bench "ple matrix only" $ nf pleEvalE mat
    ]
  , env ( return (exampleMatrix matSize :: Matrix FMPQ) ) $ \mat ->
    bgroup "FMPQ"
    [ bench "ple all" $ nf pleEvalLE mat
    , bench "ple matrix only" $ nf pleEvalE mat
    ]

  , withNModContext 7 $ \(_ :: Proxy ctxProxy) -> 
    env ( return (exampleMatrix matSize :: Matrix (NMod ctxProxy) ) ) $ \mat ->
      bgroup "NMod"
      [ bench "ple all" $ nf pleEvalLE mat
      , bench "ple matrix only" $ nf pleEvalE mat
      ]
  ]
