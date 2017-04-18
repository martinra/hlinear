{-# LANGUAGE
    FlexibleInstances
  , DataKinds
  , KindSignatures
  , MultiParamTypeClasses
  , ScopedTypeVariables
  #-}

module HLinear.Matrix.Sized
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import GHC.TypeLits ( Nat(..), KnownNat, natVal )
import Math.Structure
import Numeric.Natural ( Natural )
import Test.QuickCheck.Arbitrary
  ( Arbitrary, arbitrary, shrink )
import Test.Vector ()
import Test.SmallCheck.Series
  ( Serial, series, decDepth )

import HLinear.Matrix.Algebra ()
import HLinear.Matrix.Basic ()
import HLinear.Matrix.Definition ( Matrix(..), IsMatrix(..) )


newtype MatrixSized (nrs::Nat) (ncs::Nat) a =
  MatrixSized { fromMatrixSized :: Matrix a }

instance IsMatrix (MatrixSized nrs ncs a) a where
  toMatrix = fromMatrixSized

instance Eq a => Eq (MatrixSized nrs ncs a) where
  (MatrixSized m) == (MatrixSized m') = m == m'

instance Show a => Show (MatrixSized nrs ncs a) where
  show (MatrixSized m) = show m

--------------------------------------------------------------------------------
-- QuickCheck
--------------------------------------------------------------------------------

instance ( KnownNat nrs, KnownNat ncs, Arbitrary a )
      => Arbitrary (MatrixSized nrs ncs a)
  where
  arbitrary = do
    let nrs = natVal (Proxy :: Proxy nrs)
    let ncs = natVal (Proxy :: Proxy ncs)
    rs <- V.replicateM (fromInteger nrs) $
            V.replicateM (fromInteger ncs) arbitrary
    return $ MatrixSized $
        Matrix (fromInteger nrs) (fromInteger ncs) rs

  shrink (MatrixSized (Matrix nrs ncs rs)) =
    [ MatrixSized $ Matrix nrs ncs rs'
    | rs' <- shrink rs
    , V.length rs' == fromIntegral nrs
    , V.all ((== fromIntegral ncs) . V.length) rs'
    ]

--------------------------------------------------------------------------------
-- SmallCheck
--------------------------------------------------------------------------------

instance ( KnownNat nrs, KnownNat ncs, Monad m, Serial m a )
      => Serial m (MatrixSized nrs ncs a)
  where
  series = do
    let nrs = natVal (Proxy :: Proxy nrs)
    let ncs = natVal (Proxy :: Proxy ncs)
    rs <- V.sequence $ V.iterateN (fromInteger nrs) decDepth $
            V.sequence $ V.iterateN (fromInteger ncs) decDepth $
              decDepth $ decDepth series
    return $ MatrixSized $ Matrix (fromInteger nrs) (fromInteger ncs) rs

--------------------------------------------------------------------------------
-- Algebra
--------------------------------------------------------------------------------

instance AdditiveMagma a => AdditiveMagma (MatrixSized nrs ncs a) where
  (MatrixSized m) + (MatrixSized m') = MatrixSized $ m + m'

instance AdditiveSemigroup a => AdditiveSemigroup (MatrixSized nrs ncs a)

instance Abelian a => Abelian (MatrixSized nrs ncs a)

instance ( KnownNat nrs, KnownNat ncs, AdditiveMonoid a )
      => AdditiveMonoid (MatrixSized nrs ncs a)
  where
  zero =
    let nrs = natVal ( Proxy :: Proxy nrs )
        ncs = natVal ( Proxy :: Proxy ncs )
    in  MatrixSized $ Matrix (fromInteger nrs) (fromInteger ncs) $
          V.replicate (fromInteger nrs) $
            V.replicate (fromInteger ncs) zero

instance ( KnownNat nrs, KnownNat ncs, AdditiveGroup a )
      => AdditiveGroup (MatrixSized nrs ncs a)
  where
  negate (MatrixSized (Matrix nrs ncs rs)) =
    MatrixSized $ Matrix nrs ncs $ V.map (V.map negate) rs

instance Rng a => MultiplicativeMagma (MatrixSized nrs nrs a) where
  (MatrixSized m) * (MatrixSized m') = MatrixSized $ m * m'

instance Rng a => MultiplicativeSemigroup (MatrixSized nrs nrs a)

instance ( KnownNat nrs, Ring a )
      => MultiplicativeMonoid (MatrixSized nrs nrs a)
  where
  one =
    let nrs = natVal (Proxy :: Proxy nrs)
    in  MatrixSized $ Matrix (fromInteger nrs) (fromInteger nrs) $
          V.generate (fromInteger nrs) $ \ix ->
            V.generate (fromInteger nrs) $ \jx ->
              if ix == jx then one else zero

instance Rng a => Distributive (MatrixSized nrs nrs a)

instance Rng a => Semiring (MatrixSized nrs nrs a)

instance (KnownNat nrs, Ring a)
      => Rng (MatrixSized nrs nrs a)

instance (KnownNat nrs, Ring a)
      => Rig (MatrixSized nrs nrs a)

instance (KnownNat nrs, Ring a)
      => Ring (MatrixSized nrs nrs a)
