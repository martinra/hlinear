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

import HLinear.Matrix
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

at :: RPermute -> Int -> Int -> Int
at (RPermute p) n ix = n-1 - P.at p (n-1 - ix)


-- todo: check for Permute.at 
toMatrix :: Ring a => RPermute -> Matrix a
toMatrix p = Matrix np np $
                V.generate npZ $ \ix ->
                V.generate npZ $ \jx ->
                  if at p npZ jx == ix then one else zero
  where
    npZ = size p
    np = fromIntegral npZ 

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

-- action on vectors

newtype RPVector a = RPVector {fromRPVector :: Vector a}

-- this is partially defined (i.e. for internal use only)
instance
  MultiplicativeSemigroupLeftAction RPermute (RPVector a)
  where
  rp@(RPermute p) *. (RPVector v) = RPVector $ V.backpermute v vp'
    where
      nv = V.length v
      np = size rp
      vp = V.generate np $ \ix -> nv-1 - (p `P.at` (np-1 - ix))
      vp' = case compare nv np of
              EQ -> vp
              GT -> V.enumFromN 0 (nv-np) V.++ vp
              LT -> error "RPermute *. lVector: permutation to large"

instance MultiplicativeLeftAction RPermute (RPVector a)
