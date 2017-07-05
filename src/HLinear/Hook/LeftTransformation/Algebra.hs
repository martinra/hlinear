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
import HLinear.Hook.LeftTransformation.Basic ( fitSize )
import HLinear.Hook.LeftTransformation.Column hiding ( one, isOne )
import HLinear.Hook.LeftTransformation.Definition
import HLinear.Utility.RPermute
import qualified HLinear.Matrix.Algebra as M
import qualified HLinear.Matrix.Basic as M
import qualified HLinear.Matrix.Naive as MNaive
import qualified HLinear.Hook.LeftTransformation.Column as LTC

--------------------------------------------------------------------------------
-- permutation action
--------------------------------------------------------------------------------

instance Ring a
  => MultiplicativeSemigroupLeftAction RPermute (LeftTransformation a)
  where
  p *. lt@(LeftTransformation nrs cs)
    | pn <= nrsZ - V.length cs = LeftTransformation nrs $ fmap (p*.) cs
    | otherwise = LeftTransformationMatrix $ p *. toMatrix (fitSize pn lt)
    where
      pn = size p
      nrsZ = fromIntegral nrs
  p *. lt@(LeftTransformationMatrix m) =
    LeftTransformationMatrix $ p *. toMatrix (fitSize (size p) lt)

instance Ring a
  => MultiplicativeLeftAction RPermute (LeftTransformation a)

instance Ring a
  => MultiplicativeSemigroupRightAction RPermute (LeftTransformation a)
  where
  lt@(LeftTransformation nrs cs) .* p
    | pn <= nrsZ - V.length cs = lt
    | otherwise = LeftTransformationMatrix $ p *. toMatrix (fitSize pn lt)
    where
      pn = size p
      nrsZ = fromIntegral nrs
  lt@(LeftTransformationMatrix m) .* p =
    LeftTransformationMatrix $ toMatrix (fitSize (size p) lt) .* p

instance Ring a
  => MultiplicativeRightAction RPermute (LeftTransformation a)

--------------------------------------------------------------------------------
-- product structure
--------------------------------------------------------------------------------

instance Ring a
  => MultiplicativeMagma (LeftTransformation a)
  where
  lt@(LeftTransformation nrs cs) * lt'@(LeftTransformation nrs' cs')
    | ncs  == 0 = lt'
    | ncs' == 0 = lt
    | nrsZ - ncs >= nrs'Z =
        let ltcOnes = fmap (LTC.one nrsZ) $
                        V.enumFromN ncs (nrsZ - ncs - nrs'Z)
            cs'shifted = fmap (LTC.adjustOffset (+(nrsZ-nrs'Z))) cs' 
        in  LeftTransformation nrs $ cs <> ltcOnes <> cs'shifted
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

instance Ring a
  => MultiplicativeSemigroup (LeftTransformation a)

instance Ring a
  => MultiplicativeMonoid (LeftTransformation a)
  where
  one = LeftTransformation 0 V.empty

instance ( Ring a
         , DecidableZero a, DecidableOne (Unit a) )
  => DecidableOne (LeftTransformation a)
  where
  isOne (LeftTransformation nrs cs) =
    V.null cs
    ||
    (`all` cs) (\(LeftTransformationColumn _ a v) -> isOne a && all isZero v)

instance ( Ring a, DecidableUnit a )
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

instance ( Ring a, AdditiveMonoid b, LinearSemiringLeftAction a b )
  => MultiplicativeSemigroupLeftAction
       (LeftTransformation a) (Column b)
  where
  -- we fill the vector v with zeros from the top
  LeftTransformationMatrix m *. c@(Column v) =
    case compare nrsZ vn of
      EQ -> m *. c
      GT -> m *. Column (V.replicate (nrsZ - vn) zero <> v)
      LT -> let (v1,v2) = V.splitAt (vn-nrsZ) v
            in  Column $ v1 <> fromColumn (m *. Column v2)
    where
      nrsZ = fromIntegral $ nmbRows m
      vn = V.length v
  lt@(LeftTransformation nrs cs) *. (Column v) =
    Column $ V.foldr' applyLTC v' cs'
    where
      cs' = V.drop (nrsZ - vn) cs
      v' | nrsZ <= vn = v
         | otherwise  = V.replicate (nrsZ-vn) zero <> v
      nrsZ = fromIntegral nrs
      vn = V.length v

applyLTC
  :: ( Ring a, LinearSemiringLeftAction a b )
  => LeftTransformationColumn a -> Vector b -> Vector b
applyLTC c@(LeftTransformationColumn s a v) w
  | V.length w < V.length v = error "applyLTC: incompatible sizes"
  | otherwise = w11 `V.snoc` w12' <> w2'
      where
        (w1,w2) = V.splitAt (V.length w - V.length v) w
        w11 = V.init w1
        w12 = V.last w1
        w12' = fromUnit a *. w12
        w2' = V.zipWith (\bl br -> bl*.w12' + br) v w2

instance ( Ring a, LeftModule a b )
  => MultiplicativeLeftAction (LeftTransformation a) (Column b)

--------------------------------------------------------------------------------
-- action on LeftTransformationColumn
--------------------------------------------------------------------------------

instance Ring a
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

instance ( Ring a, DecidableUnit a )
  => MultiplicativeLeftAction
       (LeftTransformation a)
       (LeftTransformationColumn a)

--------------------------------------------------------------------------------
-- action on matrices
--------------------------------------------------------------------------------

instance Ring a
  => MultiplicativeSemigroupLeftAction
       (LeftTransformation a) (Matrix a)
  where
  lt *. (Matrix nrs ncs rs) =
    Matrix nrs ncs $ M.withRowLength ncs go
      where
        go :: forall ctx. Reifies ctx Natural => Proxy ctx -> Vector (Vector a)
        go _ = fmap M.fromRow $ fromColumn $
                 lt *. (Column $ fmap M.Row rs :: Column (M.Row ctx a))

instance Ring a
  => MultiplicativeLeftAction
       (LeftTransformation a) (Matrix a)
