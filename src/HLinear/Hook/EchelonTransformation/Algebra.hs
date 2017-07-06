module HLinear.Hook.EchelonTransformation.Algebra
where

import HLinear.Utility.Prelude
import qualified Prelude as P

import qualified Data.Vector as V

import HLinear.Hook.EchelonTransformation.Basic as ET
import HLinear.Hook.EchelonTransformation.Column
import HLinear.Hook.EchelonTransformation.Definition
import HLinear.Matrix.Algebra ()
import HLinear.Matrix.Column ( Column(..) )
import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.Utility.RPermute ( RPermute )
import qualified HLinear.Hook.EchelonTransformation.Column as ETC


--------------------------------------------------------------------------------
-- product structure 
--------------------------------------------------------------------------------

instance    Ring a
         => MultiplicativeMagma (EchelonTransformation a) where
  et@(EchelonTransformation nrs cs) * et'@(EchelonTransformation nrs' cs')
    | ncs  == 0 = et'
    | ncs' == 0 = et
    | nrs - ncs >= nrs' = 
        let etcOnes = fmap (ETC.one nrs) $
                        V.enumFromN ncs (nrs - ncs - nrs')
            cs'Shifted = fmap (ETC.adjustOffset (+(nrs-nrs'))) cs' 
        in EchelonTransformation nrs $ cs <> etcOnes <> cs'Shifted
    | nrs <= nrs' - ncs' =
        EchelonTransformation nrs' (fmap (et*.) cs') * et
    | nrs <= nrs' && nrs-ncs >= nrs'-ncs' =
        EchelonTransformation nrs' (fmap (et*.) cs')
    | otherwise =
        let (etLeftMiddle,etRight) = ET.splitAt nrs' et
            (etLeft,etMiddle) = ET.splitAt (nrs'-ncs') etLeftMiddle
        in etRight * (etMiddle * (etLeft * et'))
    where
      ncs = V.length cs
      ncs' = V.length cs'

instance    Ring a
         => MultiplicativeSemigroup (EchelonTransformation a)

instance    Ring a
         => MultiplicativeMonoid (EchelonTransformation a) where
  one = EchelonTransformation 0 V.empty

instance    ( Ring a, DecidableZero a, DecidableOne a )
         => DecidableOne (EchelonTransformation a) where
  isOne (EchelonTransformation nrs cs) = all ETC.isOne cs

instance    ( Ring a, DecidableZero a )
         => MultiplicativeGroup (EchelonTransformation a) where
  recip (EchelonTransformation nrs cs)
    | V.length cs == 1
      = let EchelonTransformationColumn _ c = V.head cs
            c' = EchelonTransformationColumn 0 (fmap negate c)
        in EchelonTransformation nrs $ V.singleton c'
    | otherwise = foldl (*) initLt $ V.reverse $ fmap recip columnLTs
        where
        initLt = EchelonTransformation nrs V.empty
        columnLTs = fmap go cs
        go (EchelonTransformationColumn _ c) =
          EchelonTransformation (1 + V.length c) $
                                V.singleton $ EchelonTransformationColumn 0 c

--------------------------------------------------------------------------------
-- action on vectors
--------------------------------------------------------------------------------

instance ( Ring a, LinearSemiringLeftAction a b )
  => MultiplicativeSemigroupLeftAction
       (EchelonTransformation a) (Column b)
  where
  -- we fill the vector v with zeros from the bottom
  lt@(EchelonTransformation nrs cs) *. (Column v) = Column $
    V.foldr' applyCol v $ V.drop (nrs - V.length v) cs
    where
      -- this assumes that vn is longer than v'
      {-# INLINE applyCol #-}
      applyCol c@(EchelonTransformationColumn o' v') vn =
         V.zipWith (\bl br -> bl*.ev + br) v' vn1
         <>
         vn2
           where
           ev = V.head vn2
           (vn1,vn2) = V.splitAt (V.length v') vn

instance ( Ring a, LeftModule a b )
  => MultiplicativeLeftAction (EchelonTransformation a) (Column b)

--------------------------------------------------------------------------------
-- action on matrices
--------------------------------------------------------------------------------

instance Ring a
  => MultiplicativeSemigroupLeftAction
       (EchelonTransformation a) (Matrix a)
  where
  et *. (Matrix nrs' ncs' rs') =
    Matrix nrs' ncs' $ fromColumn $ et *. Column rs'

instance Ring a
  => MultiplicativeLeftAction
       (EchelonTransformation a) (Matrix a)

--------------------------------------------------------------------------------
-- action on EchelonTransformationColumn
--------------------------------------------------------------------------------

instance  Ring a
  => MultiplicativeSemigroupLeftAction
      (EchelonTransformation a)
      (EchelonTransformationColumn a)
  where
  et@(EchelonTransformation nrs cs) *. etc@(EchelonTransformationColumn s v) =
    EchelonTransformationColumn s v'
    where
      etv = fromColumn $ et *. Column v
      v' = case cs V.!? (nrs - 1 - V.length v) of
             Nothing -> etv
             Just c  -> V.zipWith (+) etv (ETC.init c)

instance Ring a
  => MultiplicativeLeftAction
      (EchelonTransformation a)
      (EchelonTransformationColumn a)
