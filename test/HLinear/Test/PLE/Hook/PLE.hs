module HLinear.Test.PLE.Hook.PLE
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Maybe
import Data.Proxy
import Math.Structure
import Math.Structure.Tasty

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Vector

import HLinear.Matrix as M
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.PLE.Hook.RPermute as RP
import HLinear.PLE.PLE
import HLinear.PLE.Hook.PLE as PLE

import HLinear.Test.Utils

import Debug.Trace

hookPLEProperties :: TestTree
hookPLEProperties =
  testGroup "Hook PLE"
  [ testPropertyMatrix "recombine splitOffHook" $
      \m -> let _ = m :: Matrix Rational
            in fromMaybe True $ do
                 (PLEHook p l e, m') <- splitOffHook m
                 let m'zero = if LT.nmbCols l == 1
                              then M.zeroMatrix 1 1 `mappend` m'
                              else M.zeroMatrix (M.nmbRows m) 1 `M.blockSumRows` m'
                 let pm = RP.toMatrix p
                 let lm = LT.toMatrix l
                 let em = EF.toMatrix e
                 let d (Matrix nrs ncs _) = show [nrs,ncs]
                 return $ lm * pm * m == m'zero + em

  , testPropertyMatrix "recombine ple decomposition" $
      \m -> let pleM = ple (m :: Matrix Rational)
                p = fromMatrixPermute $ permutation pleM
                pl = left pleM
                em = echelon pleM
            in m == M.permuteRows p (pl * em)
                

  ]
