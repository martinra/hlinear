{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module HLinear.Hook.LeftTransformation.Algebra
where

import qualified Prelude as P
import HLinear.Utility.Prelude

import Data.Reflection
import qualified Data.Vector as V

import HLinear.Matrix.Column ( Column(..) )
import HLinear.Matrix.Definition ( Matrix(..), IsMatrix(..) )
import qualified HLinear.Hook.LeftTransformation.Basic as LT
import HLinear.Hook.LeftTransformation.Column hiding ( one, isOne )
import HLinear.Hook.LeftTransformation.Definition
import HLinear.Utility.RPermute
import qualified HLinear.Matrix.Algebra as M
import qualified HLinear.Matrix.Naive as MNaive
import qualified HLinear.Hook.LeftTransformation.Column as LTC

--------------------------------------------------------------------------------
-- permutation action
--------------------------------------------------------------------------------

instance Ring a
  => MultiplicativeSemigroupLeftAction RPermute (LeftTransformation a)
  where
  p *. lt@(LeftTransformation nrs cs)
    | size p <= fromIntegral nrs - V.length cs =
        LeftTransformation nrs $ fmap (p*.) cs
    | otherwise = LeftTransformationMatrix $ p *. toMatrix lt
  p *. (LeftTransformationMatrix m) = LeftTransformationMatrix (p *. m)

instance Ring a
  => MultiplicativeLeftAction RPermute (LeftTransformation a)

instance Ring a
  => MultiplicativeSemigroupRightAction RPermute (LeftTransformation a)
  where
  lt@(LeftTransformation nrs cs) .* p
    | size p <= fromIntegral nrs - V.length cs = lt
    | otherwise = LeftTransformationMatrix $ toMatrix lt .* p
  (LeftTransformationMatrix m) .* p = LeftTransformationMatrix (m .* p)

instance Ring a
  => MultiplicativeRightAction RPermute (LeftTransformation a)

--------------------------------------------------------------------------------
-- product structure
--------------------------------------------------------------------------------

instance ( Ring a, MultiplicativeGroup (Unit a) )
  => MultiplicativeMagma (LeftTransformation a)
  where
  lt@(LeftTransformation nrs cs) * lt'@(LeftTransformation nrs' cs')
    | ncs  == 0 = lt'
    | ncs' == 0 = lt
    | nrsZ - ncs >= nrs'Z =
        let ltcOnes = fmap (LTC.one nrsZ) $
                        V.enumFromN ncs (nrsZ - ncs - nrs'Z)
            cs'shifted = fmap (LTC.setLength nrsZ) cs' 
        in LeftTransformation nrs $ cs <> ltcOnes <> cs'shifted
    | nrs' >= nrs =
        let ltLeft = LeftTransformation nrs' $ fmap (lt*.) cs' 
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

instance ( Ring a, MultiplicativeGroup (Unit a) )
  => MultiplicativeSemigroup (LeftTransformation a)

instance ( Ring a, MultiplicativeGroup (Unit a) )
  => MultiplicativeMonoid (LeftTransformation a)
  where
  one = LeftTransformation 0 V.empty

instance ( Ring a, MultiplicativeGroup (Unit a)
         , DecidableZero a, DecidableOne (Unit a) )
  => DecidableOne (LeftTransformation a)
  where
  isOne (LeftTransformation nrs cs) =
    V.null cs
    ||
    (`all` cs) (\(LeftTransformationColumn _ a v) -> isOne a && all isZero v)

instance ( Ring a, DecidableUnit a, MultiplicativeGroup (Unit a) )
  => MultiplicativeGroup (LeftTransformation a)
  where
  recip (LeftTransformation nrs cs)
    | V.length cs == 1
      = let LeftTransformationColumn _ a c = V.head cs
            c' = LeftTransformationColumn 0 (recip a) (fmap ((* fromUnit a) . negate) c)
        in LeftTransformation nrs $ V.singleton c'
    | otherwise = foldl (*) initLt $ V.reverse $ fmap recip columnLTs
        where
        initLt = LeftTransformation nrs V.empty
        columnLTs = fmap go cs
        go (LeftTransformationColumn _ a c) =
          LeftTransformation (fromIntegral $ succ $ V.length c) $
                             V.singleton $ LeftTransformationColumn 0 a c
  recip (LeftTransformationMatrix m) = LeftTransformationMatrix $ MNaive.recip m

--------------------------------------------------------------------------------
-- determinant
--------------------------------------------------------------------------------

det :: Ring a => LeftTransformation a -> a
det (LeftTransformation _ cs) = foldl' (*) one $ fmap LTC.head cs
-- to define the determinant, fall back to a naive
-- algorithm, so that we don't have circular imports
det (LeftTransformationMatrix m) = MNaive.det m

--------------------------------------------------------------------------------
-- action on columns
--------------------------------------------------------------------------------

instance ( Ring a, MultiplicativeGroup (Unit a), LinearSemiringLeftAction a b )
  => MultiplicativeSemigroupLeftAction
       (LeftTransformation a) (Column b)
  where
  -- we fill the vector v with zeros from the top
  lt@(LeftTransformation nrs cs) *. (Column v) = Column $
    V.foldr' applyCol v $ V.drop nrsDiff cs
    where
    nv = V.length v
    nrsDiff = fromIntegral nrs - nv

    -- this assumes that vn is longer than v'
    {-# INLINE applyCol #-}
    applyCol c@(LeftTransformationColumn s' a' v') vn =
       V.init vn1 `V.snoc` av
       <>
       V.zipWith (\bl br -> bl*.av + br) v' vn2
         where
         av = fromUnit a' *. V.last vn1
         (vn1,vn2) = V.splitAt (V.length vn - V.length v') vn

instance ( Ring a, MultiplicativeGroup (Unit a), LeftModule a b )
  => MultiplicativeLeftAction (LeftTransformation a) (Column b)

--------------------------------------------------------------------------------
-- action on LeftTransformationColumn
--------------------------------------------------------------------------------

instance ( Ring a, MultiplicativeGroup (Unit a) )
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
      nza1recip = fromUnit $ maybe one (recip . LTC.headUnit) c1

      a' = maybe a ((*a) . LTC.headUnit) c1
      v' = case c1 of
             Just c1' -> V.zipWith (\bc bv -> bc + bv*nza1recip)
                           (LTC.tail c1') (fromColumn $ lt *. Column v)
             Nothing  -> fromColumn $ lt *. Column v

instance ( Ring a, MultiplicativeGroup (Unit a), DecidableUnit a )
  => MultiplicativeLeftAction
       (LeftTransformation a)
       (LeftTransformationColumn a)

--------------------------------------------------------------------------------
-- action on matrices
--------------------------------------------------------------------------------

instance ( Ring a, MultiplicativeGroup (Unit a) )
  => MultiplicativeSemigroupLeftAction
       (LeftTransformation a) (Matrix a)
  where
  lt *. (Matrix nrs ncs rs) =
    Matrix nrs ncs $ M.withRowLength ncs go
      where
        go :: forall ctx. Reifies ctx Natural => Proxy ctx -> Vector (Vector a)
        go _ = fmap M.fromRow $ fromColumn $
                 lt *. (Column $ fmap M.Row rs :: Column (M.Row ctx a))

instance ( Ring a, MultiplicativeGroup (Unit a) )
  => MultiplicativeLeftAction
       (LeftTransformation a) (Matrix a)
