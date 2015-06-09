{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.PLE.Hook.LeftTransformation.Instances
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Applicative ( (<$>) )
import Control.Arrow ( first )
import Data.Maybe
import Data.Vector ( Vector(..) )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.PLE.Hook.LeftTransformation.Basic
import HLinear.PLE.Hook.LeftTransformation.Column
import HLinear.PLE.Hook.ReversePermute


instance MultiplicativeSemigroupLeftAction ReversePermute (LeftTransformation a) where
  p *. (LeftTransformation nrs cs) = LeftTransformation nrs $ V.map (p*.) cs


instance DivisionRing a => MultiplicativeMagma (LeftTransformation a) where
  lt@(LeftTransformation nrs cs) * (LeftTransformation nrs' cs') =
    LeftTransformation (max nrs nrs') $
      csLeft V.++ V.map (LeftTransformation nrs' csMiddleRight *.) cs' V.++ csRight
    where
    ncs = V.length cs
    ncs' = V.length cs'
    nrsDiff = min 0 $ fromIntegral nrs - fromIntegral nrs'
    (csLeft, csMiddleRight) = V.splitAt nrsDiff cs
    (csMiddle, csRight) = V.splitAt ncs' csMiddleRight

instance DivisionRing a => MultiplicativeSemigroup (LeftTransformation a)
instance DivisionRing a => MultiplicativeMonoid (LeftTransformation a) where
  one = LeftTransformation 0 V.empty
instance    ( DivisionRing a, DecidableZero a, DecidableOne a )
         => DecidableOne (LeftTransformation a) where
  isOne (LeftTransformation nrs cs) =
    V.null cs
    ||
    (`all` cs) (\(LeftTransformationColumn _ a v) -> isOne (fromNonZero a) && all isZero v)

instance DivisionRing a => MultiplicativeGroup (LeftTransformation a) where
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

instance    (DivisionRing a, LinearSemiringLeftAction a b)
         => MultiplicativeSemigroupLeftAction
              (LeftTransformation a) (Vector b)
  where
  -- we fill the vector v with zeros from the top
  (LeftTransformation nrs cs) *. v =
    V.foldr' applyCol v $ V.drop nrsDiff cs
    where
    nv = length v
    nrsDiff = fromIntegral nrs - nv

    -- this assumes that vn is longer than v'
    applyCol c@(LeftTransformationColumn s' a' v') vn =
       V.init vn1 `V.snoc` av
       V.++
       V.zipWith (\bl br -> bl*.av + br) v' vn2
         where
         av = fromNonZero a' *. V.last vn1
         (vn1,vn2) = V.splitAt (V.length vn - V.length v') vn

instance    (DivisionRing a, LeftModule a b)
         => MultiplicativeLeftAction (LeftTransformation a) (Vector b)


instance    DivisionRing a
         => MultiplicativeSemigroupLeftAction
             (LeftTransformation a)
             (LeftTransformationColumn a)
  where
  lt@(LeftTransformation nrs cs) *. (LeftTransformationColumn s a v) =
    LeftTransformationColumn s a' v'
    where
    nv = V.length v
    nvDiff = fromIntegral nrs - nv

    c1 = cs V.!? (nvDiff - 1)

    a' = maybe a ((*a) . ltcHeadNonZero) c1
    v' = go (lt *. v)
           where
           go = case c1 of
                  Just c1' -> V.zipWith (\bc bv -> bv + bc*nza') (ltcTail c1')
                  Nothing  -> id
           nza' = fromNonZero a'

instance    DivisionRing a
         => MultiplicativeLeftAction
             (LeftTransformation a)
             (LeftTransformationColumn a)
