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
import HLinear.Hook.LeftTransformation.Basic ( fitSize, drop )
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
  {-# INLINABLE (*.) #-}
  p *. lt@(LeftTransformation nrs cs)
    | pn <= nrs - V.length cs = LeftTransformation nrs $ fmap (p*.) cs
    | otherwise = LeftTransformationMatrix $ p *. toMatrix (fitSize pn lt)
    where
      pn = size p
  p *. lt@(LeftTransformationMatrix m) =
    LeftTransformationMatrix $ p *. toMatrix (fitSize (size p) lt)

instance Ring a
  => MultiplicativeLeftAction RPermute (LeftTransformation a)

instance Ring a
  => MultiplicativeSemigroupRightAction RPermute (LeftTransformation a)
  where
  {-# INLINABLE (.*) #-}
  lt@(LeftTransformation nrs cs) .* p
    | pn <= nrs - V.length cs = lt
    | otherwise = LeftTransformationMatrix $ p *. toMatrix (fitSize pn lt)
    where
      pn = size p
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
  {-# INLINABLE (*) #-}
  lt@(LeftTransformation nrs cs) * lt'@(LeftTransformation nrs' cs')
    | ncs  == 0 = fitSize nrs lt'
    | ncs' == 0 = fitSize nrs' lt
    | nrs - ncs >= nrs' =
        let ltcOnes = fmap (LTC.one nrs) $
                        V.enumFromN ncs (nrs - ncs - nrs')
            cs'shifted = fmap (LTC.adjustOffset (+(nrs-nrs'))) cs' 
        in  LeftTransformation nrs $ cs <> ltcOnes <> cs'shifted
    | nrs' >= nrs =
        let ltLeft = LeftTransformation nrs' $ fmap (lt*.) cs' 
            ltRight = LT.drop (nrs - (nrs' - ncs')) lt
        in ltLeft * ltRight
    | otherwise =
        let (ltLeft,ltRight) = LT.splitAt (nrs - nrs') lt
        in ltLeft * (ltRight * lt')
    where
      ncs = V.length cs
      ncs' = V.length cs'
  -- todo: to optimize computation of HNF insert this special case
  -- lt@(LeftTransformation nrs cs) * lt'@(LeftTransformationMatrix m) =
  lt * lt' =
    case compare nrs nrs' of
      EQ -> LeftTransformationMatrix $ toMatrix lt * toMatrix lt'
      GT -> LeftTransformationMatrix $ toMatrix lt * toMatrix (fitSize nrs lt')
      LT -> LeftTransformationMatrix $ toMatrix (fitSize nrs' lt) * toMatrix lt'
    where
      nrs = nmbRows lt
      nrs' = nmbRows lt'

instance Ring a
  => MultiplicativeSemigroup (LeftTransformation a)

instance Ring a
  => MultiplicativeMonoid (LeftTransformation a)
  where
  one = LeftTransformation 0 V.empty

instance ( Ring a, DecidableZero a, DecidableOne (Unit a) )
  => DecidableOne (LeftTransformation a)
  where
  isOne (LeftTransformation nrs cs) =
    V.null cs
    ||
    (`all` cs) (\(LeftTransformationColumn _ a v) -> isOne a && all isZero v)

instance ( Ring a, DecidableUnit a )
  => MultiplicativeGroup (LeftTransformation a)
  where
  {-# INLINABLE recip #-}
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
          LeftTransformation (1 + V.length c) $
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
  {-# INLINABLE (*.) #-}
  LeftTransformationMatrix m *. c@(Column v) =
    case compare nrs vn of
      EQ -> m *. c
      GT -> m *. Column (V.replicate (nrs-vn) zero <> v)
      LT -> let (v1,v2) = V.splitAt (vn-nrs) v
            in  Column $ v1 <> fromColumn (m *. Column v2)
    where
      nrs = nmbRows m
      vn = V.length v
  lt@(LeftTransformation nrs cs) *. (Column v) =
    Column $ V.foldr' applyLTC v' cs'
    where
      cs' = V.drop (nrs-vn) cs
      v' | nrs <= vn = v
         | otherwise  = V.replicate (nrs-vn) zero <> v
      vn = V.length v

      {-# INLINE applyLTC #-}
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
  {-# INLINABLE (*.) #-}
  lt@(LeftTransformation nrs cs) *. ltc@(LeftTransformationColumn s a v) =
    LeftTransformationColumn s a' v'
    where
      nv = V.length v

      -- the column of lt that corresponds to the head of ltc
      c = cs V.!? (nrs - (nv+1))

      cHeadRecip = maybe one (fromUnit . recip . LTC.headUnit) c

      a' = maybe a ((*a) . LTC.headUnit) c
      ltv = fromColumn $ drop (nrs-nv) lt *. Column v
      v' = case c of
             Just c' -> V.zipWith (\bc bv -> bc + bv*cHeadRecip)
                          (LTC.tail c') ltv
             Nothing -> ltv
  (LeftTransformationMatrix _) *. _ =
    error "LeftTransformationMatrix *. LeftTransformationColumn"

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
  {-# INLINABLE (*.) #-}
  lt *. (Matrix nrs ncs rs) =
    Matrix nrs ncs $ M.withRowLength ncs go
      where
        go :: forall ctx. Reifies ctx Int => Proxy ctx -> Vector (Vector a)
        go _ = fmap M.fromRow $ fromColumn $
                 lt *. (Column $ fmap M.Row rs :: Column (M.Row ctx a))

instance Ring a
  => MultiplicativeLeftAction
       (LeftTransformation a) (Matrix a)
