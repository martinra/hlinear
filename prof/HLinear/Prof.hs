{-# LANGUAGE
    FlexibleInstances
  #-}

module Main
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.DeepSeq ( NFData(..) )
import Data.Maybe
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural

import HFlint.FMPQ

import HLinear.Matrix as M
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.PLE.Hook.RPermute as RP
import HLinear.PLE.Hook.PLE
import HLinear.PLE.PLE

exampleMatrix :: ( Fractional a, Ring a, NFData a )
              => Natural -> Matrix a
exampleMatrix n = seq (rnf m) m
  where
  nZ = fromIntegral n
  Right m = M.fromVectors' n n $
    V.generate nZ $ \ix ->
    V.generate nZ $ \jx ->
      let ix' = fromIntegral ix
          jx' = fromIntegral jx
          n'  = fromIntegral n
      in (ix'^2 + 2) P./ ((n'-jx')^3 + 1)

pleEvalE :: ( DecidableZero a, DivisionRing a )
         => Matrix a -> Matrix a
pleEvalE = echelon . ple

main = do
  let mat = exampleMatrix 200 :: Matrix FMPQ
  seq (rnf $ pleEvalE mat) $ print "DONE"
