module HLinear.PLE.Hook.EchelonForm.Container
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

--import HLinear.EchelonForm.Basic ()
import HLinear.PLE.Hook.EchelonForm.Definition
import HLinear.PLE.Hook.EchelonForm.Row as EFR


instance Functor EchelonForm where
  fmap f (EchelonForm nrs ncs rs) = EchelonForm nrs ncs $ V.map (fmap f) rs

instance Foldable EchelonForm where
  foldl f a (EchelonForm nrs ncs rs) = V.foldl (\a' r -> foldl f a' r) a rs
  foldr f a (EchelonForm nrs ncs rs) = V.foldr (\r a' -> foldr f a' r) a rs

instance Traversable EchelonForm where
  traverse f (EchelonForm nrs ncs rs) =
    EchelonForm nrs ncs <$> sequenceA (V.map (traverse f) rs)

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
      V.replicate (maxnrsZ - nrs''Z) (zeroEFR maxncs)
      V.++ rs'' V.++
      V.replicate (minnrsB - nrs''B) (zeroEFR maxncs)
