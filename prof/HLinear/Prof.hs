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
import HLinear.PLE.Hook as Hk
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.ReducedEchelonForm as REF
import HLinear.PLE

import HLinear.Bench.Example ( uniformRandomMatrixQQbdLE )


main = do
    let snum = 10
        nden = 5 
        sden = 4 
    m <- fmap fromRational <$>
         uniformRandomMatrixQQbdLE 20 30 snum nden sden :: IO (Matrix FMPQ)

    let pleReducedEchelonFFMatrixFMPQ =
            EF.toMatrix . snd . REF.reducedEchelonForm
          . Hk.echelonForm . unPLEDecomposition
          . pleDecompositionFoldUnfoldFractionFree
    let ref = pleReducedEchelonFFMatrixFMPQ m
    print $ seq ( rnf ref ) "DONE"
   
--  m <- uniformRandomMatrix 10 20 20 :: IO (Matrix Rational)
--  let ef = echelon $ ple m
--  print ef
--  print "DONE"
--  print $ seq ( rnf ef ) "DONE"
