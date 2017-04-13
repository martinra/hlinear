{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.PLE.Hook.LeftTransformation.Algebra
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Maybe
import Data.Proxy
import Data.Reflection
import Data.Vector ( Vector(..) )
import Math.Structure
import Numeric.Natural ( Natural )
import qualified Data.Vector as V

import HLinear.Matrix ( Matrix(..) )
import HLinear.PLE.Hook.LeftTransformation.Basic as LT
import HLinear.PLE.Hook.LeftTransformation.Column
import HLinear.PLE.Hook.LeftTransformation.Definition
import HLinear.PLE.Hook.PLMatrix
import HLinear.PLE.Hook.RPermute
import qualified HLinear.Matrix.Algebra as M
import qualified HLinear.PLE.Hook.LeftTransformation.Column as LTC


-- partially defined permutation action
instance MultiplicativeSemigroupLeftAction
           RPermute
           (LeftTransformation a)
  where
  p *. (LeftTransformation nrs cs) = LeftTransformation nrs $ V.map (p*.) cs

-- product structure

instance    ( DivisionRing a, DecidableZero a )
         => MultiplicativeMagma (LeftTransformation a) where
  lt@(LeftTransformation nrs cs) * lt'@(LeftTransformation nrs' cs')
    | ncs  == 0 = lt'
    | ncs' == 0 = lt
    | nrsZ - ncs >= nrs'Z =
        let ltcOnes = V.map (LTC.identityLTColumn nrsZ) $
                        V.enumFromN ncs (nrsZ - ncs - nrs'Z)
            cs'shifted = V.map (LTC.setLength nrsZ) cs' 
        in LeftTransformation nrs $ cs V.++ ltcOnes V.++ cs'shifted
    | nrs' >= nrs =
        let ltLeft = LeftTransformation nrs' $ V.map (lt*.) cs' 
            ltRight = LT.drop (nrsZ - (nrs'Z - ncs')) lt
        in ltLeft * ltRight
    | otherwise =
        let (ltLeft,ltRight) = LT.splitAt (nrsZ - nrs'Z) lt
        in ltLeft * (ltRight * lt')
    where
      maxnrs = max nrs nrs'
      minnrs = min nrs nrs'
      ncs = V.length cs
      ncs' = V.length cs'

      nrsZ = fromIntegral nrs
      nrs'Z = fromIntegral nrs'
      maxnrsZ = fromIntegral maxnrs
      minnrsZ = fromIntegral minnrs 

instance    ( DivisionRing a, DecidableZero a )
         => MultiplicativeSemigroup (LeftTransformation a)

instance    ( DivisionRing a, DecidableZero a )
         => MultiplicativeMonoid (LeftTransformation a) where
  one = LeftTransformation 0 V.empty

instance    ( DivisionRing a, DecidableZero a, DecidableOne a )
         => DecidableOne (LeftTransformation a) where
  isOne (LeftTransformation nrs cs) =
    V.null cs
    ||
    (`all` cs) (\(LeftTransformationColumn _ a v) -> isOne (fromNonZero a) && all isZero v)

instance    ( DivisionRing a, DecidableZero a )
         => MultiplicativeGroup (LeftTransformation a) where
  recip (LeftTransformation nrs cs)
    | V.length cs == 1
      = let LeftTransformationColumn _ a c = V.head cs
            a' = fromNonZero a
            c' = LeftTransformationColumn 0 (recip a) (V.map ((*a') . negate) c)
        in LeftTransformation nrs $ V.singleton c'
    | otherwise = foldl (*) initLt $ V.reverse $ V.map recip columnLTs
        where
        initLt = LeftTransformation nrs V.empty
        columnLTs = V.map go cs
        go (LeftTransformationColumn _ a c) =
          LeftTransformation (fromIntegral $ succ $ V.length c) $
                             V.singleton $ LeftTransformationColumn 0 a c

-- action on RVector

-- incoherent against
-- instance Semiring a =>
--   MultiplicativeSemigroupLeftAction a (RVector a)
-- should not occur: LinearSemiringLeftAction a (LeftTransformation a)
instance  {-# INCOHERENT #-}
            ( DivisionRing a, DecidableZero a, LinearSemiringLeftAction a b )
         => MultiplicativeSemigroupLeftAction
              (LeftTransformation a) (PLVector b)
  where
  -- we fill the vector v with zeros from the top
  lt@(LeftTransformation nrs cs) *. (PLVector v) = PLVector $
    V.foldr' applyCol v $ V.drop nrsDiff cs
    where
    nv = V.length v
    nrsDiff = fromIntegral nrs - nv

    -- this assumes that vn is longer than v'
    {-# INLINE applyCol #-}
    applyCol c@(LeftTransformationColumn s' a' v') vn =
       V.init vn1 `V.snoc` av
       V.++
       V.zipWith (\bl br -> bl*.av + br) v' vn2
         where
         av = fromNonZero a' *. V.last vn1
         (vn1,vn2) = V.splitAt (V.length vn - V.length v') vn

instance    ( DivisionRing a, DecidableZero a, LeftModule a b )
         => MultiplicativeLeftAction (LeftTransformation a) (PLVector b)

-- action on LeftTransformationColumn

instance    ( DivisionRing a, DecidableZero a )
         => MultiplicativeSemigroupLeftAction
             (LeftTransformation a)
             (LeftTransformationColumn a)
  where
  lt@(LeftTransformation nrs cs) *. ltc@(LeftTransformationColumn s a v) =
    LeftTransformationColumn s a' v'
    where
      nv = V.length v
      nvDiff = fromIntegral nrs - nv
  
      c1 = cs V.!? (nvDiff-1)
      nza1recip = fromNonZero $ maybe one (recip . LTC.headNonZero) c1

      a' = maybe a ((*a) . LTC.headNonZero) c1
      ltv = fromPLVector $ lt *. PLVector v
      v' = case c1 of
             Just c1' -> V.zipWith (\bc bv -> bc + bv*nza1recip)
                           (LTC.tail c1') ltv
             Nothing  -> ltv

instance    ( DivisionRing a, DecidableZero a )
         => MultiplicativeLeftAction
             (LeftTransformation a)
             (LeftTransformationColumn a)

-- action on Matrix

instance    ( DivisionRing a, DecidableZero a )
         => MultiplicativeSemigroupLeftAction
              (LeftTransformation a) (PLMatrix a)
  where
  lt *. (PLMatrix (Matrix nrs ncs rs)) =
    PLMatrix $ Matrix nrs ncs $ M.withRowLength ncs go
      where
        go :: forall ctx. Reifies ctx Natural => Proxy ctx -> Vector (Vector a)
        go _ = V.map M.fromRow $ fromPLVector $
                 lt *. (PLVector $ V.map M.Row rs :: PLVector (M.Row ctx a))

instance    ( DivisionRing a, DecidableZero a ) 
         => MultiplicativeLeftAction
              (LeftTransformation a) (PLMatrix a)

-- apply
--   :: DivisionRing a
--   => WeakLeftTransformation a -> Matrix a -> Matrix a
-- apply wlt (Matrix nrs ncs rs) =
--   Matrix nrs ncs $
--   V.map RV.toCurrentVector $ RV.toCurrentVector $
--   WLT.apply wlt $
--   RV.RVector $ V.map RV.RVector rs
-- 
-- 
-- 
-- -- fixme: this should be applied to PLVector
-- -- application to RVector
-- 
-- apply
--   :: ( DivisionRing a, LinearSemiringLeftAction a b )
--   => WeakLeftTransformation a -> RVector b -> RVector b
-- apply lt@(WeakLeftTransformation nrs cs) (RVector v) = RVector $
--     V.foldr' applyCol v $ V.drop nrsDiff cs
--     where
--     nv = V.length v
--     nrsDiff = fromIntegral nrs - nv
-- 
--     -- this assumes that vn is longer than v'
--     {-# INLINE applyCol #-}
--     applyCol c@(WeakLeftTransformationColumn s' a' v') vn =
--        V.init vn1 `V.snoc` av
--        V.++
--        V.zipWith (\bl br -> bl*.av + br) v' vn2
--          where
--          av = a' *. V.last vn1
--          (vn1,vn2) = V.splitAt (V.length vn - V.length v') vn
-- 
