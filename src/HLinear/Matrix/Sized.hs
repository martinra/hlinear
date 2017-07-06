{-# LANGUAGE
    ScopedTypeVariables
  #-}

module HLinear.Matrix.Sized
where

import HLinear.Utility.Prelude

import GHC.TypeLits ( Nat(..), KnownNat, natVal )
import Test.QuickCheck.Arbitrary
  ( Arbitrary, arbitrary, shrink )
import Test.Vector ()
import Test.SmallCheck.Series
  ( Serial, series, decDepth )
import qualified Data.Vector as V

import HLinear.Matrix.Algebra ()
import HLinear.Matrix.Basic ( diagonal )
import HLinear.Matrix.Definition ( Matrix(..), IsMatrix(..) )
import HLinear.Matrix.Invertible ( MatrixInvertible )
import qualified HLinear.Matrix.Basic as M
import qualified HLinear.Matrix.Naive as Naive


--------------------------------------------------------------------------------
-- MatrixSized
--------------------------------------------------------------------------------

newtype MatrixSized (nrs::Nat) (ncs::Nat) a =
  MatrixSized { fromMatrixSized :: Matrix a }

instance IsMatrix (MatrixSized nrs ncs a) a where
  toMatrix = fromMatrixSized

deriving instance Eq (Matrix a) => Eq (MatrixSized nrs ncs a)

deriving instance Show (Matrix a) => Show (MatrixSized nrs ncs a)

--------------------------------------------------------------------------------
-- MatrixInvertibleSized
--------------------------------------------------------------------------------

newtype MatrixInvertibleSized (nrs::Nat) a =
  MatrixInvertibleSized { fromMatrixInvertibleSized :: MatrixInvertible a }

instance IsMatrix (MatrixInvertibleSized nrs a) a where
  toMatrix = fromUnit . fromMatrixInvertibleSized

deriving instance Eq (Matrix a) => Eq (MatrixInvertibleSized nrs a)

deriving instance Show (Matrix a) => Show (MatrixInvertibleSized nrs a)

--------------------------------------------------------------------------------
-- QuickCheck
--------------------------------------------------------------------------------

instance ( KnownNat nrs, KnownNat ncs, Arbitrary a )
      => Arbitrary (MatrixSized nrs ncs a)
  where
  arbitrary = do
    let nrs = fromIntegral $ natVal (Proxy :: Proxy nrs)
    let ncs = fromIntegral $ natVal (Proxy :: Proxy ncs)
    rs <- V.replicateM nrs $ V.replicateM ncs arbitrary
    return $ MatrixSized $ Matrix nrs ncs rs

  shrink (MatrixSized (Matrix nrs ncs rs)) =
    [ MatrixSized $ Matrix nrs ncs rs'
    | rs' <- shrink rs
    , V.length rs' == nrs
    , V.all ((==ncs) . V.length) rs'
    ]

--------------------------------------------------------------------------------
-- SmallCheck
--------------------------------------------------------------------------------

instance ( KnownNat nrs, KnownNat ncs, Monad m, Serial m a )
      => Serial m (MatrixSized nrs ncs a)
  where
  series = do
    let nrs = fromIntegral $ natVal (Proxy :: Proxy nrs)
    let ncs = fromIntegral $ natVal (Proxy :: Proxy ncs)
    rs <- V.sequence $ V.iterateN nrs decDepth $
            V.sequence $ V.iterateN ncs decDepth $
              decDepth $ decDepth series
    return $ MatrixSized $ Matrix nrs ncs rs

--------------------------------------------------------------------------------
-- additive structure
--------------------------------------------------------------------------------

deriving instance AdditiveMagma (Matrix a)
  => AdditiveMagma (MatrixSized nrs ncs a)

deriving instance AdditiveSemigroup (Matrix a)
  => AdditiveSemigroup (MatrixSized nrs ncs a)

deriving instance Abelian (Matrix a)
  => Abelian (MatrixSized nrs ncs a)


instance ( KnownNat nrs, KnownNat ncs, AdditiveMonoid a )
      => AdditiveMonoid (MatrixSized nrs ncs a)
  where
  zero =
    let nrs = fromIntegral $ natVal ( Proxy :: Proxy nrs )
        ncs = fromIntegral $ natVal ( Proxy :: Proxy ncs )
    in  MatrixSized $ Matrix nrs ncs $
          V.replicate nrs $ V.replicate ncs zero

instance ( KnownNat nrs, KnownNat ncs, AdditiveMonoid a, DecidableZero a )
      => DecidableZero (MatrixSized nrs ncs a)
  where
    isZero = isZero . fromMatrixSized

instance ( KnownNat nrs, KnownNat ncs, AdditiveGroup a )
      => AdditiveGroup (MatrixSized nrs ncs a)
  where
    (MatrixSized m) - (MatrixSized m') = MatrixSized $ m - m'
    negate = MatrixSized . negate . fromMatrixSized

--------------------------------------------------------------------------------
-- multiplicative structure
--------------------------------------------------------------------------------

deriving instance MultiplicativeMagma (Matrix a)
  => MultiplicativeMagma (MatrixSized nrs nrs a)

deriving instance MultiplicativeSemigroup (Matrix a)
  => MultiplicativeSemigroup (MatrixSized nrs nrs a)

instance ( MultiplicativeMagma (Matrix a), Commutative a )
  => Commutative (MatrixSized 1 1 a)

instance ( KnownNat nrs, Ring a )
      => MultiplicativeMonoid (MatrixSized nrs nrs a)
  where
  one =
    let nrs = fromIntegral $ natVal (Proxy :: Proxy nrs)
    in  MatrixSized $ diagonal $ V.replicate nrs one


deriving instance MultiplicativeMagma (Matrix a)
  => MultiplicativeMagma (NonZero (MatrixSized nrs nrs a))

deriving instance MultiplicativeSemigroup (Matrix a)
  => MultiplicativeSemigroup (NonZero (MatrixSized nrs nrs a))

deriving instance ( MultiplicativeMagma (Matrix a), Commutative a )
  => Commutative (NonZero (MatrixSized 1 1 a))

deriving instance ( KnownNat nrs, Ring a )
      => MultiplicativeMonoid (NonZero (MatrixSized nrs nrs a))


instance
     ( KnownNat nrs, Ring a, DecidableUnit a )
  => MultiplicativeGroup (Unit (MatrixSized nrs nrs a))
  where
  recip (Unit (MatrixSized m)) = Unit $ MatrixSized $ Naive.recip m

--------------------------------------------------------------------------------
-- ring structure
--------------------------------------------------------------------------------

instance Rng a => Distributive (MatrixSized nrs nrs a)

instance Rng a => Semiring (MatrixSized nrs nrs a)

instance (KnownNat nrs, Ring a)
      => Rng (MatrixSized nrs nrs a)

instance (KnownNat nrs, Ring a, DecidableUnit a)
      => Rig (MatrixSized nrs nrs a)

instance (KnownNat nrs, Ring a, DecidableUnit a)
      => Ring (MatrixSized nrs nrs a)

--------------------------------------------------------------------------------
-- group structure
--------------------------------------------------------------------------------

deriving instance MultiplicativeMagma (Matrix a)
  => MultiplicativeMagma (MatrixInvertibleSized nrs a)

deriving instance MultiplicativeSemigroup (Matrix a)
  => MultiplicativeSemigroup (MatrixInvertibleSized nrs a)

instance ( MultiplicativeMagma (Matrix a), Commutative a )
  => Commutative (MatrixInvertibleSized 1 a)

instance ( KnownNat nrs, Ring a )
      => MultiplicativeMonoid (MatrixInvertibleSized nrs a)
  where
  one =
    let nrs = fromIntegral $ natVal (Proxy :: Proxy nrs)
    in  MatrixInvertibleSized $ Unit $ diagonal $ V.replicate nrs one

instance ( KnownNat nrs, Ring a, DecidableZero a, DecidableOne a )
  => DecidableOne (MatrixInvertibleSized nrs a)
  where
  isOne (MatrixInvertibleSized (Unit m)) = isOne m

instance ( KnownNat nrs, Ring a, DecidableUnit a )
      => MultiplicativeGroup (MatrixInvertibleSized nrs a)
  where
  recip (MatrixInvertibleSized (Unit m)) = MatrixInvertibleSized $ Unit $ Naive.recip m

--------------------------------------------------------------------------------
-- group action
--------------------------------------------------------------------------------

instance MultiplicativeSemigroup (Matrix a)
  => MultiplicativeSemigroupLeftAction (MatrixSized nrs nrs a) (MatrixSized nrs ncs a)
  where
    (MatrixSized m) *. (MatrixSized m') = MatrixSized $ m * m'

instance ( KnownNat nrs, Ring a, MultiplicativeSemigroup (Matrix a) )
  => MultiplicativeLeftAction (MatrixSized nrs nrs a) (MatrixSized nrs ncs a)

instance MultiplicativeSemigroup (Matrix a)
  => MultiplicativeSemigroupRightAction (MatrixSized nrs nrs a) (MatrixSized nrs ncs a)
  where
    (MatrixSized m') .* (MatrixSized m) = MatrixSized $ m * m'

instance ( KnownNat nrs, Ring a, MultiplicativeSemigroup (Matrix a) )
  => MultiplicativeRightAction (MatrixSized nrs nrs a) (MatrixSized nrs ncs a)
