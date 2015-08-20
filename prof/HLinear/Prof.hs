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
-- import HLinear.PLE.Hook.EchelonForm as EF
-- import HLinear.PLE.Hook.LeftTransformation as LT
-- import HLinear.PLE.Hook.RPermute as RP
-- import HLinear.PLE.Hook.PLE
-- import HLinear.PLE.PLE

import HLinear.Bench.Examples ( uniformRandomMatrix, toFMPQMat )


main = return ()
--  m <- uniformRandomMatrix 10 20 20 :: IO (Matrix Rational)
--  let ef = echelon $ ple m
--  print ef
--  print "DONE"
--  print $ seq ( rnf ef ) "DONE"
