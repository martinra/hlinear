{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  #-}

module HLinear.Utility.RPermute
where

--------------------------------------------------------------------------------
-- RPermute models permutations that act on vectors that are indexed in reverse
-- order. More precisely, the indices of vectors are ... 3 2 1 instead of
-- 1 2 3 ...
--------------------------------------------------------------------------------

import qualified Prelude as P
import HLinear.Utility.Prelude

import qualified Data.Vector as V
import Data.Permute ( Permute )
import qualified Data.Permute as P
import Test.QuickCheck
import Test.SmallCheck.Series

import HLinear.Utility.Permute ()
import HLinear.Matrix.Definition
import HLinear.Matrix.Basic hiding ( zero, one )


newtype RPermute = RPermute Permute
  deriving ( Show, NFData )

--------------------------------------------------------------------------------
-- Eq, Ord instances
--------------------------------------------------------------------------------

instance Eq RPermute where
  p == p' = compare p p' == EQ

instance Ord RPermute where
  compare rp@(RPermute p) rp'@(RPermute p')
    | V.null cmps = EQ
    | otherwise   = V.head cmps
    where
      np = P.size p
      np' = P.size p'
      maxnp = max np np'
      cmps = V.dropWhile (==EQ) $ V.zipWith compare cp cp'
      cp = toVectorSize maxnp rp :: Vector Int
      cp' = toVectorSize maxnp rp' :: Vector Int

---------------------------------------------------------------------------------
-- QuickCheck and SmallCheck
---------------------------------------------------------------------------------

instance Arbitrary RPermute where
  arbitrary = RPermute <$> arbitrary
  shrink (RPermute p) = RPermute <$> shrink p

instance Monad m => Serial m RPermute where
  series = RPermute <$> series

---------------------------------------------------------------------------------
-- attributes
---------------------------------------------------------------------------------

size :: RPermute -> Int
size (RPermute p) = P.size p

at :: RPermute -> Int -> Int -> Int
at (RPermute p) n ix = n-1 - P.at p (n-1 - ix)

---------------------------------------------------------------------------------
-- creation and conversion
---------------------------------------------------------------------------------

rpermute :: Int -> RPermute
rpermute n = RPermute $ P.permute n

fromTransposition :: Int -> (Int,Int) -> RPermute
fromTransposition n (a,b) =
  RPermute $ P.swapsPermute n [(n-1 - a, n-1 - b)]

toPermute :: RPermute -> Permute
toPermute (RPermute p) =
  P.swapsPermute n [(n-a,n-b) | (a,b) <- P.swaps p]
    where
    n = P.size p

instance Ring a => IsMatrix RPermute a where
  toMatrix p = Matrix np np $
                  V.generate npZ $ \ix ->
                  V.generate npZ $ \jx ->
                    if at p npZ jx == ix then one else zero
    where
      npZ = size p
      np = fromIntegral npZ 

toVector :: Num a => RPermute -> Vector a
toVector (RPermute p) = V.map (fromIntegral . P.at p) $
  V.enumFromStepN (pred np) (-1) np
    where
      np = P.size p 

toVectorSize :: Num a => Int -> RPermute -> Vector a
toVectorSize maxnp rp@(RPermute p) =
  V.map fromIntegral $ V.enumFromStepN (maxnp-1) (-1) (maxnp - np)
  V.++
  toVector rp
    where
      np = P.size p 

--------------------------------------------------------------------------------
-- product structure
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- action on vectors and matrices
--------------------------------------------------------------------------------

instance
  MultiplicativeSemigroupLeftAction RPermute (Vector a)
  where
  rp@(RPermute p) *. v = V.backpermute v vp'
    where
      nv = V.length v
      np = size rp
      pi = P.inverse p
      vp = V.generate np $ \ix -> nv-1 - (pi `P.at` (np-1 - ix))
      vp' = case compare nv np of
              EQ -> vp
              GT -> V.enumFromN 0 (nv-np) `mappend` vp
              LT -> error "RPermute *. Vector: permutation to large"

instance MultiplicativeLeftAction RPermute (Vector a)

instance MultiplicativeSemigroupRightAction RPermute (Vector a)
  where
  v .* p = recip p *. v

instance MultiplicativeRightAction RPermute (Vector a)


instance MultiplicativeSemigroupLeftAction RPermute (Matrix a)
  where
  p *. (Matrix nrs ncs rs) = Matrix nrs ncs $ p *. rs

instance MultiplicativeLeftAction RPermute (Matrix a)

instance MultiplicativeSemigroupRightAction RPermute (Matrix a)
  where
  (Matrix nrs ncs rs) .* p = Matrix nrs ncs $ V.map (.* p) rs

instance MultiplicativeRightAction RPermute (Matrix a)
