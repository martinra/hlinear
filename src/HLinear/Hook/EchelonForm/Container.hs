module HLinear.Hook.EchelonForm.Container
where

import Prelude ()
import HLinear.Utility.Prelude

import qualified Data.Vector as V

import HLinear.Hook.EchelonForm.Definition
import HLinear.Hook.EchelonForm.Row as EFR


instance Functor EchelonForm where
  fmap = fmapDefault

instance Foldable EchelonForm where
  foldMap = foldMapDefault

instance Traversable EchelonForm where
  traverse f (EchelonForm nrs ncs rs) = EchelonForm nrs ncs <$> traverse (traverse f) rs

zipWith
  :: ( AdditiveMonoid a, AdditiveMonoid b )
  => (a -> b -> c) -> EchelonForm a -> EchelonForm b -> EchelonForm c
zipWith f (EchelonForm nrs ncs rs) (EchelonForm nrs' ncs' rs') =
  EchelonForm maxnrs maxncs $
    V.zipWith (EFR.zipWith f)
      (complete nrsZ nrsB rs) (complete nrs'Z nrs'B  rs')
  where
    nrsZ = fromIntegral nrs
    nrs'Z = fromIntegral nrs'
    maxnrsZ = max nrsZ nrs'Z
    maxnrs = fromIntegral maxnrsZ

    nrsB = nrsZ - V.length rs
    nrs'B = nrs'Z - V.length rs'
    minnrsB = min nrsB nrs'B

    maxncs = max ncs ncs'

    complete :: Int -> Int -> Vector (EchelonFormRow a) -> Vector (EchelonFormRow a)
    complete nrs''Z nrs''B rs'' =
      V.replicate (maxnrsZ - nrs''Z) (EFR.zero maxncs)
      V.++ rs'' V.++
      V.replicate (minnrsB - nrs''B) (EFR.zero maxncs)
