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
import HLinear.VVMatrix hiding ( (!), (!?) )
import HLinear.VVMatrix.Utils
import HLinear.VVMatrix.Definition ( VVMatrix(..) )


instance MultiplicativeSemigroupLeftAction ReversePermute (LeftTransformationColumn a) where
  p *. (LeftTransformationColumn s a v)
    | sizeRP p > V.length v = error "to large permutation"
    | otherwise              = LeftTransformationColumn s a (p *. v)

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

instance    (DivisionRing a, LeftModule a b)
         => MultiplicativeSemigroupLeftAction
              (LeftTransformation a) (Vector b)
  where
  -- we fill the vector v with zeros from the top
  (LeftTransformation nrs cs) *. v =
    V.foldr' go v $ V.drop (fromIntegral nrs - nv) cs
    where
    nv = length v
    -- this assumes that vn is longer than v'
    go c@(LeftTransformationColumn s' a' v') vn =
       V.init vn1
       `V.snoc`
       (nza' *. V.last vn1)
       V.++
       V.zipWith (\bl br -> bl*nza'*.br) v' vn2
         where
         nza' = fromNonZero a'
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
    nvDiff = fromIntegral nrs - fromIntegral s - nv

    c1 = cs V.!? (nvDiff - 1)

    a' = maybe a ((*a) . ltcHeadNonZero) c1
    alRecip = maybe one ltcHeadRecip c1
    v' = V.zipWith (\bl br -> bl + br*alRecip) (maybe V.empty ltcTail c1) $
         ltDrop nvDiff lt *. v

instance    DivisionRing a
         => MultiplicativeLeftAction
             (LeftTransformation a)
             (LeftTransformationColumn a)
