module HLinear.PLE.Hook.LeftTransformation.Container
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      , length, zipWith
                      )

import qualified Data.Vector as V
import Data.Vector ( Vector(..) )
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.Matrix ( toMatrix, Column, fromColumns, toColumns )
import qualified HLinear.Matrix.Column as MC
import HLinear.PLE.Hook.LeftTransformation.Basic
import HLinear.PLE.Hook.LeftTransformation.Definition
import HLinear.PLE.Hook.LeftTransformation.Column ( LeftTransformationColumn )
import qualified HLinear.PLE.Hook.LeftTransformation.Column as LTC


instance Functor LeftTransformation where
  fmap f (LeftTransformation nrs cs) = LeftTransformation nrs $ V.map (fmap f) cs
  fmap f (LeftTransformationMatrix m) = LeftTransformationMatrix $ fmap f m

instance Foldable LeftTransformation where
  foldl f a (LeftTransformation nrs cs) = V.foldl (\a' c -> foldl f a' c) a cs
  foldl f a (LeftTransformationMatrix m) = V.foldl (\a' c -> foldl f a' c) a $ toColumns m

  foldr f a (LeftTransformation nrs cs) = V.foldr (\c a' -> foldr f a' c) a cs
  foldr f a (LeftTransformationMatrix m) = V.foldr (\c a' -> foldr f a' c) a $ toColumns m


instance Traversable LeftTransformation where
  traverse f (LeftTransformation nrs cs) =
    LeftTransformation nrs <$> sequenceA (V.map (traverse f) cs)
  traverse f (LeftTransformationMatrix m) =
    LeftTransformationMatrix <$> fromColumns <$> sequenceA (V.map (traverse f) $ toColumns m)

zipWith
  :: ( DecidableZero a, DecidableZero b, Ring a, Ring b )
  => (a -> b -> c) -> LeftTransformation a -> LeftTransformation b -> LeftTransformation c
zipWith f (LeftTransformation nrs cs) (LeftTransformation nrs' cs') =
  LeftTransformation maxnrs $
    V.zipWith (LTC.zipWith f) (complete maxncs cs) (complete maxncs cs')
  where
    maxnrs = max nrs nrs'
    ncs = V.length cs
    ncs' = V.length cs'
    maxncs = max ncs ncs'

    complete ncs'' cs'' =
      cs'' V.++
      V.map ( \jx -> LTC.one jx (succ jx) )
        (V.enumFromN ncs'' (maxncs - V.length cs''))
zipWith f (LeftTransformationMatrix m) (LeftTransformationMatrix m') =
  LeftTransformationMatrix $ fromColumns $
    V.zipWith (MC.zipWith f) (toColumns m) (toColumns m')
zipWith f lt@(LeftTransformation _ _) lt'@(LeftTransformationMatrix _) =
  zipWith f (LeftTransformationMatrix $ toMatrix lt) lt'
zipWith f lt@(LeftTransformationMatrix _) lt'@(LeftTransformation _ _) =
  zipWith f lt (LeftTransformationMatrix $ toMatrix lt')
