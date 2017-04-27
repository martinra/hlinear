module HLinear.Test.Utility.Vector
where

import qualified Data.Vector as V
import Math.Structure

import HLinear.Matrix


matrixActionOnTopVector
  :: ( IsMatrix a b, AdditiveMonoid b
     , MultiplicativeSemigroupLeftAction a (Vector b) )
  => Natural -> a -> Vector b -> Bool
matrixActionOnVector nrs a v
  | nv <= nrs =
      let v' = v `mappend` V.replicate (nrs-nv) zero
          m = toMatrix a
      in  m *. v' == a *. v
  | otherwise =
      let (vt,vb) = v.splitAt nrs v
          m = toMatrix a
      in  m *. vt `mappend` vb == a *. v

matrixActionOnBottomVector
  :: ( IsMatrix a b, AdditiveMonoid b
     , MultiplicativeSemigroupLeftAction a (Vector b) )
  => Natural -> a -> Vector b -> Bool
matrixActionOnVector nrs a v
  | nv <= nrs =
      let v' = V.replicate (nrs-nv) zero `mappend` v
          m = toMatrix a
      in  m *. v' == a *. v
  | otherwise =
      let (vt,vb) = v.splitAt (nv-nrs) v
          m = toMatrix a
      in  vt `mappend` m *. vb == a *. v
