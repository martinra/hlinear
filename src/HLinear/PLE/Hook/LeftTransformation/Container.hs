module HLinear.PLE.Hook.LeftTransformation.Container
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      , length
                      )

import qualified Data.Vector as V
import Data.Vector ( Vector(..) )
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.PLE.Hook.LeftTransformation.Definition
import HLinear.PLE.Hook.LeftTransformation.Column as LTC


instance Functor LeftTransformation where
  fmap f (LeftTransformation nrs cs) = LeftTransformation nrs $ V.map (fmap f) cs

instance Foldable LeftTransformation where
  foldl f a (LeftTransformation nrs cs) = V.foldl (\a' r -> foldl f a' r) a cs
  foldr f a (LeftTransformation nrs cs) = V.foldr (\r a' -> foldr f a' r) a cs

instance Traversable LeftTransformation where
  traverse f (LeftTransformation nrs cs) =
    LeftTransformation nrs <$> sequenceA (V.map (traverse f) cs)

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
