{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.PLE.Hook.EchelonTransformation.Algebra
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Vector ( Vector(..) )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.Matrix.Definition
import HLinear.PLE.Hook.EchelonTransformation.Basic as ET
import qualified HLinear.PLE.Hook.EchelonTransformation.Column as ETC
import HLinear.PLE.Hook.EchelonTransformation.Column
import HLinear.PLE.Hook.RPermute


-- product structure 

instance    DivisionRing a
         => MultiplicativeMagma (EchelonTransformation a) where
  et@(EchelonTransformation nrs cs) * et'@(EchelonTransformation nrs' cs')
    | ncs  == 0 = et'
    | ncs' == 0 = et
    | nrsZ - ncs >= nrs'Z = 
        let etcOnes = V.map (ETC.identityColumn nrsZ) $
                        V.enumFromN nrs'Z (nrsZ - ncs - nrs'Z)
            cs'Shifted = V.map (ETC.setLength nrsZ) cs' 
        in EchelonTransformation nrs $  cs V.++ etcOnes V.++ cs'Shifted
    | nrsZ <= nrs'Z - ncs' =
        EchelonTransformation nrs' (V.map (et*.) cs') * et
    | nrs <= nrs' && nrsZ-ncs >= nrs'Z-ncs' =
        EchelonTransformation nrs' (V.map (et*.) cs')
    | otherwise =
        let (etLeftMiddle,etRight) = ET.splitAt nrs'Z et
            (etLeft,etMiddle) = ET.splitAt (nrs'Z-ncs') etLeftMiddle
        in etRight * (etMiddle * (etLeft * et'))
    where
      ncs = V.length cs
      ncs' = V.length cs'

      nrsZ = fromIntegral nrs
      nrs'Z = fromIntegral nrs'

instance    DivisionRing a
         => MultiplicativeSemigroup (EchelonTransformation a)

instance    DivisionRing a
         => MultiplicativeMonoid (EchelonTransformation a) where
  one = EchelonTransformation 0 V.empty

instance    ( DivisionRing a, DecidableZero a, DecidableOne a )
         => DecidableOne (EchelonTransformation a) where
  isOne (EchelonTransformation nrs cs) = ETC.isIdentityColumn `all` cs

instance    ( DivisionRing a, DecidableZero a )
         => MultiplicativeGroup (EchelonTransformation a) where
  recip (EchelonTransformation nrs cs)
    | V.length cs == 1
      = let EchelonTransformationColumn _ c = V.head cs
            c' = EchelonTransformationColumn 0 (V.map negate c)
        in EchelonTransformation nrs $ V.singleton c'
    | otherwise = foldl (*) initLt $ V.reverse $ V.map recip columnLTs
        where
        initLt = EchelonTransformation nrs V.empty
        columnLTs = V.map go cs
        go (EchelonTransformationColumn _ c) =
          EchelonTransformation (fromIntegral $ succ $ V.length c) $
                                V.singleton $ EchelonTransformationColumn 0 c

-- action on Vector

-- incoherent against
-- instance Semiring a =>
--   MultiplicativeSemigroupLeftAction a (Vector a)
-- should not occur: LinearSemiringLeftAction a (EchelonTransformation a)
instance {-# INCOHERENT #-}
            ( DivisionRing a, LinearSemiringLeftAction a b )
         => MultiplicativeSemigroupLeftAction
              (EchelonTransformation a) (Vector b)
  where
  -- we fill the vector v with zeros from the bottom
  lt@(EchelonTransformation nrs cs) *. v =
    V.foldr' applyCol v $ V.drop nrsDiff cs
    where
      nv = V.length v
      nrsDiff = fromIntegral nrs - nv
  
      -- this assumes that vn is longer than v'
      {-# INLINE applyCol #-}
      applyCol c@(EchelonTransformationColumn o' v') vn =
         V.zipWith (\bl br -> bl*.ev + br) v' vn1
         V.++
         vn2
           where
           ev = V.head vn2
           (vn1,vn2) = V.splitAt (V.length v') vn

instance    ( DivisionRing a, LeftModule a b )
         => MultiplicativeLeftAction (EchelonTransformation a) (Vector b)

-- action on EchelonTransformationColumn

instance    DivisionRing a
         => MultiplicativeSemigroupLeftAction
             (EchelonTransformation a)
             (EchelonTransformationColumn a)
  where
  et@(EchelonTransformation nrs cs) *. etc@(EchelonTransformationColumn s v) =
    EchelonTransformationColumn s v'
    where
      nv = fromIntegral $ V.length v
      ncs = V.length cs
      nrsZ = fromIntegral nrs

      etv = et*.v
      v' = case cs V.!? (nrsZ-1-nv) of
             Nothing -> etv
             Just c  -> V.zipWith (+) etv (ETC.init c)

instance    DivisionRing a
         => MultiplicativeLeftAction
             (EchelonTransformation a)
             (EchelonTransformationColumn a)

-- action on Matrix

instance    DivisionRing a
         => MultiplicativeSemigroupLeftAction
              (EchelonTransformation a) (Matrix a)
  where
  et *. (Matrix nrs' ncs' rs') =
    Matrix nrs' ncs' $ et *. rs'

instance    DivisionRing a
         => MultiplicativeLeftAction
              (EchelonTransformation a) (Matrix a)
