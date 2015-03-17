module HLinear.VVMatrix.Algebra
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import qualified Data.Vector as V

import Math.Algebra.Structure


instance AdditiveMagma a => AdditiveMagma (VVMatrix a) where
  (VVMatrix nrs ncs rs) + (VVMatrix nrs' ncs' rs')
    | nrs /= nrs' = error "HLinear.VVMatrix +: number of rows does not coinside"
    | ncs /= ncs' = error "HLinear.VVMatrix +: number of columns does not coinside"
    | otherwise = VVMatrix nrs ncs $ V.zipWith (V.zipWith (+)) rs rs' 

instance Abelean a => Abelean (VVMatrix a)
instance AdditiveSemigroup a => AdditiveSemigroup (VVMatrix a)


instance    ( MultiplicativeMagma a, AdditiveMonoid a )
         => MultiplicativeMagma (VVMatrix a) where
  (VVMatrix nrs ncs rs) * (VVMatrix nrs' ncs' rs')
-- todo: assert these are square matrices and let everything else work by
-- module structures
    | ncs /= nrs' = error "HLinear.VVMatrix *: incompatible number of rows and columns"
    | otherwise = VVMatrix nrs ncs' $ if ncs /= 0 then rsNew else rsDef
      where
      rsNew = `V.map` rs $ \r -> V.foldr1' (V.zipWith (+)) $
                                 V.zipWith (\a -> V.map (a*)) r rs'
      rsDef = V.replicate nrs $ V.replicate ncs' zero

instance MultiplicativeSemigroup a => MultiplicativeSemigroup (VVMatrix a)


instance Semiring a => Semiring (VVMatrix a)
