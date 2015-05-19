{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module HLinear.PLE.Hook.ReversePermute
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import qualified Data.Vector as V
import Data.Vector ( Vector(..) )
import Data.Permute

import Math.Structure


newtype ReversePermute = ReversePermute Permute

reversePermute :: Int -> ReversePermute
reversePermute n = ReversePermute $ permute n

fromTransposition :: Int -> (Int,Int) -> ReversePermute
fromTransposition n ab = ReversePermute $ swapsPermute n [ab]

fromReversePermute :: ReversePermute -> Permute
fromReversePermute (ReversePermute p) =
  swapsPermute n [(n-a,n-b) | (a,b) <- swaps p]
    where
    n = size p

sizeRP :: ReversePermute -> Int
sizeRP (ReversePermute p) = size p


instance MultiplicativeMagma ReversePermute where
  (ReversePermute p) * (ReversePermute p') =
    ReversePermute $ swapsPermute n $ swaps p ++ swaps p'
      where
      n = max (size p) (size p')
instance MultiplicativeSemigroup ReversePermute
instance MultiplicativeMonoid ReversePermute where
  one = ReversePermute $ permute 0
instance DecidableOne ReversePermute where
  isOne (ReversePermute p) = undefined p
instance MultiplicativeGroup ReversePermute where
  recip (ReversePermute p) = ReversePermute (undefined p)


instance MultiplicativeSemigroupLeftAction ReversePermute (Vector a) where
  (ReversePermute p) *. v =
    V.backpermute v $ V.generate nv $ \ix -> nv - 1 - (p `at` ix)
      where
      nv = V.length v
instance MultiplicativeLeftAction ReversePermute (Vector a)
