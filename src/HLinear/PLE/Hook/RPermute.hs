{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module HLinear.PLE.Hook.RPermute
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import qualified Data.Vector as V
import Data.Vector ( Vector(..) )
import Data.Permute ( Permute )
import qualified Data.Permute as P

import Math.Structure

import HLinear.BRMatrix.RVector ( RVector(RVector) )

-- right permutations, i.e. action on vectors with indices ... 3 2 1
-- instead of 1 2 3 ...
newtype RPermute = RPermute Permute

rpermute :: Int -> RPermute
rpermute n = RPermute $ P.permute n

fromTransposition :: Int -> (Int,Int) -> RPermute
fromTransposition n ab = RPermute $ P.swapsPermute n [ab]

toPermute :: RPermute -> Permute
toPermute (RPermute p) =
  P.swapsPermute n [(n-a,n-b) | (a,b) <- P.swaps p]
    where
    n = P.size p

size :: RPermute -> Int
size (RPermute p) = P.size p

-- product structure

instance MultiplicativeMagma RPermute where
  rp@(RPermute p) * rp'@(RPermute p') =
    RPermute $ P.swapsPermute n $ P.swaps p ++ P.swaps p'
      where
      n = max (size rp) (size rp')

instance MultiplicativeSemigroup RPermute

instance MultiplicativeMonoid RPermute where
  one = rpermute 0

instance DecidableOne RPermute where
  isOne rp@(RPermute p) =  V.all (\ix -> p `P.at` ix == ix)
                                 (V.enumFromTo 0 (size rp-1))
                                         
instance MultiplicativeGroup RPermute where
  recip (RPermute p) = RPermute $ P.inverse p

-- action on RVector

-- incoherent against
-- instance Semiring a =>
--   MultiplicativeSemigroupLeftAction a (RVector a)
-- should not occur: instance Semiring RPermute
instance {-# INCOHERENT #-}
  MultiplicativeSemigroupLeftAction RPermute (RVector a)
  where
  rp@(RPermute p) *. (RVector v) = RVector $ V.backpermute v' vp'
    where
      nv = V.length v
      np = np
      vp = V.generate np $ \ix -> np-1 - (p `P.at` ix)
      (v',vp') = case compare nv np of
                   EQ -> (v,vp)
                   GT -> (v,V.enumFromStepN nv (-1) (nv-np) V.++ vp)

instance MultiplicativeLeftAction RPermute (RVector a)
