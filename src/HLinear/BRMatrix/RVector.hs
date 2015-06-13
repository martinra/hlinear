{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
  #-}

module HLinear.BRMatrix.RVector
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Arrow ( second )
import Control.DeepSeq ( NFData, rnf )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural


-- RVector a is the inductive limit
--   liminv a^n
-- of length n vectors with entries in a, where the embeddings
--   a^n \hookrightarrow a^{n+n'}
-- are mapping to the right, filling the left with zeros.
newtype RVector a = RVector {toCurrentVector :: Vector a}

minimizeSize :: DecidableZero a => RVector a -> RVector a
minimizeSize = RVector . toShortestVector

fromRVectorUnsafe :: DecidableZero a => Natural -> RVector a -> Vector a
fromRVectorUnsafe nrs rv@(RVector v) =
  case compare nrsZ nv of
    EQ -> v
    GT -> error "RVector:toVectorUnsafe cannot convert"
    LT -> V.replicate (nrsZ - nv) zero V.++ v
  where
    nrsZ = fromIntegral nrs
    nv = V.length v

toShortestVector :: DecidableZero a => RVector a -> Vector a
toShortestVector = V.dropWhile isZero . toCurrentVector

currentLength :: RVector a -> Natural
currentLength = fromIntegral . V.length . toCurrentVector

lift :: (Vector a -> b) -> RVector a -> b
lift f = f . toCurrentVector

liftRV :: (Vector a -> Vector b) -> RVector a -> RVector b
liftRV f = RVector . lift f

_lift2 :: (Vector a -> Vector b -> c)
       -> RVector a -> RVector b
       -> (Either (Vector a) (Vector b), c)
_lift2 f (RVector v) (RVector w) =
  case compare nv nw of
    EQ -> (Left V.empty, f v w)
    GT -> (Left v1, f v2 w)
    LT -> (Right w1, f v w2)
  where
    nv = V.length v
    nw = V.length w
    (v1,v2) = V.splitAt (nv-nw) v
    (w1,w2) = V.splitAt (nw-nv) w
     
lift2Discard :: (Vector a -> Vector b -> c)
             -> RVector a -> RVector b -> c
lift2Discard f v w = snd $ _lift2 f v w

lift2RVDiscard :: (Vector a -> Vector b -> Vector c)
               -> RVector a -> RVector b -> RVector c
lift2RVDiscard f v w = RVector $ lift2Discard f v w

lift2Overlap :: (Vector a -> Vector a -> Vector a -> b)
               -> RVector a -> RVector a -> b
lift2Overlap f v w = fvw
  where
    (a,fvw) = _lift2 f' v w
    f' = f $ case a of Left l -> l; Right r -> r

-- Equality and Show

instance ( Eq a, DecidableZero a ) => Eq (RVector a) where
  (==) = lift2Overlap $ \zs v w ->
           V.all isZero zs && V.all (uncurry (==)) (V.zip v w)

deriving instance NFData a => NFData (RVector a)

-- entry access

(!) :: AdditiveMonoid a => RVector a -> Int -> a
(!) (RVector v) ix
  | ix < V.length v = v V.! ix
  | otherwise = zero

-- Additive structure

instance AdditiveMagma a => AdditiveMagma (RVector a) where
  (+) = lift2Overlap $ \l v w ->
          RVector $ l V.++ V.zipWith (+) v w
          
instance AdditiveSemigroup a => AdditiveSemigroup (RVector a)
instance AdditiveMonoid a => AdditiveMonoid (RVector a) where
  zero = RVector V.empty
instance DecidableZero a => DecidableZero (RVector a) where
  isZero = V.all isZero . toCurrentVector
instance Abelian a => Abelian (RVector a)

instance AdditiveGroup a => AdditiveGroup (RVector a) where
  negate (RVector v) = RVector $ V.map negate v

-- Left action by scalars

instance    Semiring a
         => MultiplicativeSemigroupLeftAction a (RVector a) where
  a *. (RVector v) = RVector $ V.map (a*.) v

instance Rig a => MultiplicativeLeftAction a (RVector a)
instance Rng a => LinearSemiringLeftAction a (RVector a)
instance Ring a => LeftModule a (RVector a)

-- Right action by scalars

instance    Semiring a
         => MultiplicativeSemigroupRightAction a (RVector a) where
  (RVector v) .* a = RVector $ V.map (.*a) v

instance Rig a => MultiplicativeRightAction a (RVector a)
instance Rng a => LinearSemiringRightAction a (RVector a)
instance Ring a => RightModule a (RVector a)
