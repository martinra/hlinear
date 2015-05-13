{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  #-}

module HLinear.VVMatrix.Algebra.Sized
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import Data.Composition ( (.:) )
import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy(..) )
import qualified Data.Vector as V
import Math.Structure
import GHC.TypeLits ( Nat, KnownNat, natVal )

import HLinear.VVMatrix.Algebra.TypeUnion
import HLinear.VVMatrix.Definition
import HLinear.VVMatrix.Basic
import HLinear.VVMatrix.Creation
import HLinear.VVMatrix.Utils


instance AdditiveMonoid a => AdditiveMagma (SizedVVMatrix nrs ncs a) where
  (SizedVVMatrix m) + (SizedVVMatrix m') = SizedVVMatrix (m + m')

instance    ( AdditiveMonoid a, Abelian a )
         => Abelian (SizedVVMatrix nrs ncs a)
instance AdditiveMonoid a => AdditiveSemigroup (SizedVVMatrix nrs ncs a)

instance    (KnownNat nrs, KnownNat ncs, AdditiveMonoid a)
         => AdditiveMonoid (SizedVVMatrix nrs ncs a) where
  zero = SizedVVMatrix $ Zero (Just nrs) (Just ncs)
    where
    nrs = fromInteger $ natVal ( Proxy :: Proxy nrs )
    ncs = fromInteger $ natVal ( Proxy :: Proxy ncs )

instance    (KnownNat nrs, KnownNat ncs, AdditiveGroup a)
         => AdditiveGroup (SizedVVMatrix nrs ncs a) where
  negate (SizedVVMatrix m) = SizedVVMatrix (negate m)
  (SizedVVMatrix m) - (SizedVVMatrix m') = SizedVVMatrix (m - m')
  

instance Rng a => MultiplicativeMagma (SizedVVMatrix nrs nrs a) where
  (SizedVVMatrix m) * (SizedVVMatrix m') = SizedVVMatrix (m*m')

instance Rng a => MultiplicativeSemigroup (SizedVVMatrix nrs nrs a)

instance    (KnownNat nrs, Ring a)
         => MultiplicativeMonoid (SizedVVMatrix nrs nrs a) where
  one = SizedVVMatrix $ One (Just nrs) one
    where
    nrs = fromInteger $ natVal ( Proxy :: Proxy nrs )

instance (KnownNat nrs, Rng a  ) => Distributive (SizedVVMatrix nrs nrs a)
instance (KnownNat nrs, Rng a  ) => Semiring (SizedVVMatrix nrs nrs a)
instance (KnownNat nrs, Rng a  ) => Rng (SizedVVMatrix nrs nrs a)
instance (KnownNat nrs, Ring a ) => Rig (SizedVVMatrix nrs nrs a)
instance (KnownNat nrs, Ring a ) => Ring (SizedVVMatrix nrs nrs a)


instance {-# INCOHERENT #-} Rng a
  => MultiplicativeSemigroupLeftAction (SizedVVMatrix nrs nrs a) (SizedVVMatrix nrs ncs a) where
  (SizedVVMatrix m) *. (SizedVVMatrix m') = SizedVVMatrix (m *. m')
instance {-# INCOHERENT #-} Rng a
  => MultiplicativeSemigroupRightAction (SizedVVMatrix ncs ncs a) (SizedVVMatrix nrs ncs a) where
  (SizedVVMatrix m) .* (SizedVVMatrix m') = SizedVVMatrix (m .* m')

instance {-# INCOHERENT #-} (KnownNat nrs, Rng a)
  => LinearSemiringLeftAction
       (SizedVVMatrix nrs nrs a) (SizedVVMatrix nrs ncs a)
instance {-# INCOHERENT #-} (KnownNat ncs, Rng a)
  => LinearSemiringRightAction
       (SizedVVMatrix ncs ncs a) (SizedVVMatrix nrs ncs a)

instance {-# INCOHERENT #-} (KnownNat nrs, Ring a)
  => MultiplicativeLeftAction
       (SizedVVMatrix nrs nrs a) (SizedVVMatrix nrs ncs a)
instance {-# INCOHERENT #-} (KnownNat ncs, Ring a)
  => MultiplicativeRightAction
       (SizedVVMatrix ncs ncs a) (SizedVVMatrix nrs ncs a)

instance {-# INCOHERENT #-} (KnownNat nrs, KnownNat ncs, Ring a)
  => LeftModule (SizedVVMatrix nrs nrs a) (SizedVVMatrix nrs ncs a)
instance {-# INCOHERENT #-} (KnownNat nrs, KnownNat ncs, Ring a)
  => RightModule (SizedVVMatrix ncs ncs a) (SizedVVMatrix nrs ncs a)


instance    Semiring a
         => MultiplicativeSemigroupLeftAction a (SizedVVMatrix nrs ncs a) where
  a *. (SizedVVMatrix m) = SizedVVMatrix (a *. m)
instance    Semiring a
         => MultiplicativeSemigroupRightAction a (SizedVVMatrix nrs ncs a) where
  (SizedVVMatrix m) .* a = SizedVVMatrix (m .* a)

instance Rng a => LinearSemiringLeftAction a (SizedVVMatrix nrs ncs a)
instance Rng a => LinearSemiringRightAction a (SizedVVMatrix nrs ncs a)

instance Ring a => MultiplicativeLeftAction a (SizedVVMatrix nrs ncs a)
instance Ring a => MultiplicativeRightAction a (SizedVVMatrix nrs ncs a)

instance    (KnownNat nrs, KnownNat ncs, Ring a)
         => LeftModule a (SizedVVMatrix nrs ncs a)
instance    (KnownNat nrs, KnownNat ncs, Ring a)
         => RightModule a (SizedVVMatrix nrs ncs a)
instance    (KnownNat nrs, KnownNat ncs, Commutative a, Ring a)
         => Module a (SizedVVMatrix nrs ncs a)

instance    (KnownNat nrs, Ring a)
         => LeftAlgebra a (SizedVVMatrix nrs nrs a)
instance    (KnownNat nrs, Ring a)
         => RightAlgebra a (SizedVVMatrix nrs nrs a)
instance    (KnownNat nrs, Commutative a, Ring a)
         => Algebra a (SizedVVMatrix nrs nrs a)
