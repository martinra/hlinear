{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  #-}

module HLinear.VVMatrix.Algebra.TypeUnion
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

import HLinear.VVMatrix.Definition
import HLinear.VVMatrix.Basic
import HLinear.VVMatrix.Creation
import HLinear.VVMatrix.Utils


-- We consider VVMatrix as a union of all matrices of arbitrary size. In
-- particular, all mathematically possible multiplications are allowed, but at
-- runtime exceptions might be raised, if dimensions do not match.

instance AdditiveMonoid a => AdditiveMagma (VVMatrix a) where
  (VVMatrix nrs ncs rs) + (VVMatrix nrs' ncs' rs') =
    VVMatrix (cmbDim' nrs nrs') (cmbDim' ncs ncs') $
             V.zipWith (V.zipWith (+)) rs rs'

  (Zero nrs ncs) + (Zero nrs' ncs') =
    Zero (cmbDimMMay' nrs nrs') (cmbDimMMay' ncs ncs')
  (Zero nrs ncs) + (One nrs' a') =
    One (cmbDimMMay' ncs $ cmbDimMMay' nrs nrs') a'
  (Zero nrs ncs) + (VVMatrix nrs' ncs' rs') =
    VVMatrix (cmbDimMay' nrs' nrs) (cmbDimMay' ncs' ncs) rs'
  m + m'@(Zero _ _)            = m' + m

  (One nrs a) + (One nrs' a')  = One (cmbDimMMay' nrs nrs') (a + a')
  m@(One nrs a) + m'@(VVMatrix nrs' ncs' rs') =
    (forceSize nrs' ncs' m) + m'
  m + m'@(One _ _)             = m' + m

instance ( AdditiveMonoid a, Abelian a ) => Abelian (VVMatrix a)
instance AdditiveMonoid a => AdditiveSemigroup (VVMatrix a)

instance AdditiveMonoid a => AdditiveMonoid (VVMatrix a) where
  zero = Zero Nothing Nothing

instance DecidableZero a => DecidableZero (VVMatrix a) where
  isZero (Zero _ _)        = True
  isZero (One _ a)         = isZero a
  isZero (VVMatrix _ _ rs) = V.all (V.all isZero) rs

instance AdditiveGroup a => AdditiveGroup (VVMatrix a) where
  negate (VVMatrix nrs ncs rs)
    = VVMatrix nrs ncs $ V.map (V.map negate) rs
  negate m@(Zero _ _) = m
  negate (One nrs a)  = One nrs (negate a)

  (VVMatrix nrs ncs rs) - (VVMatrix nrs' ncs' rs') =
    VVMatrix (cmbDim' nrs nrs') (cmbDim' ncs ncs') $
             V.zipWith (V.zipWith (-)) rs rs'

  (Zero nrs ncs) - (Zero nrs' ncs') =
    Zero (cmbDimMMay' nrs nrs') (cmbDimMMay' ncs ncs')
  (Zero nrs ncs) - (One nrs' a') =
    One (cmbDimMMay' ncs $ cmbDimMMay' nrs nrs') (negate a')
  (Zero nrs ncs) - (VVMatrix nrs' ncs' rs') =
    VVMatrix (cmbDimMay' nrs' nrs) (cmbDimMay' ncs' ncs) $
      V.map (V.map negate) rs'
  m - m'@(Zero _ _)            = m' + m

  (One nrs a) - (One nrs' a')  = One (cmbDimMMay' nrs nrs') (a - a')
  m@(One nrs a) - m'@(VVMatrix nrs' ncs' rs') =
    (forceSize nrs' ncs' m) - m'
  m - m'@(One _ _)             = m + negate m'


-- We have to allow multiplication of matrices of arbitrary size. If we
-- would only allow multiplication of square matrices here, the left and right
-- action of VVMatrix a on itself would be unsafely incoherent: The semantics
-- would change according to the type of the calling function.
instance Rng a => MultiplicativeMagma (VVMatrix a) where
  (VVMatrix nrs ncs rs) * (VVMatrix nrs' ncs' rs')
    = seq d $ case d of
        0 -> Zero (Just nrs) (Just ncs')
        _ -> VVMatrix nrs ncs' $
                     (`V.map` rs) $ \r ->
                       V.foldr1' (+) $
                       V.zipWith (\a r' -> V.map (a*) r') r rs'
      where d = cmbDim' ncs nrs'

  (Zero nrs ncs) * (Zero nrs' ncs') =
    seq (cmbDimMMay' ncs nrs') $ Zero nrs ncs'
  (Zero nrs ncs) * (One nrs' _) =
    seq (cmbDimMMay' ncs nrs') $ Zero nrs nrs'
  (One nrs _) * (Zero nrs' ncs') =
    seq (cmbDimMMay' nrs nrs') $ Zero nrs ncs'
  (Zero nrs ncs) * (VVMatrix nrs' ncs' _) =
    seq (cmbDimMay' nrs' ncs) $ Zero nrs (Just ncs')
  (VVMatrix nrs ncs _) * (Zero nrs' ncs') =
    seq (cmbDimMay' ncs nrs') $ Zero (Just nrs) ncs'

  (One nrs a) * (VVMatrix nrs' ncs' rs') =
    seq (cmbDimMay' nrs' nrs) $
    VVMatrix nrs' ncs' $ V.map (V.map (a*)) rs'

  (VVMatrix nrs ncs rs) * (One nrs' a') =
    seq (cmbDimMay' ncs nrs') $
    VVMatrix nrs ncs $ V.map (V.map (*a')) rs

instance Rng a => MultiplicativeSemigroup (VVMatrix a)

instance Ring a => MultiplicativeMonoid (VVMatrix a) where
  one = One Nothing one

instance    ( DecidableZero a, DecidableOne a, Ring a )
         => DecidableOne (VVMatrix a) where
  isOne (Zero _ _)        = False
  isOne (One _ a)         = isOne a
  isOne (VVMatrix nrs ncs rs)
    | nrs /= ncs = False
    | otherwise  = V.all id $ (`V.imap` rs) $ \ix r ->
                   V.all id $ (`V.imap` r) $ \jx e ->
                     if ix==jx then isOne e else isZero e

instance Rng a => Distributive (VVMatrix a)
instance Rng a => Semiring (VVMatrix a)
instance Rng a => Rng (VVMatrix a)
instance Ring a => Rig (VVMatrix a)
instance Ring a => Ring (VVMatrix a)


instance Semiring a => MultiplicativeSemigroupLeftAction a (VVMatrix a) where
  a *. (Zero nrs ncs) = Zero nrs ncs
  a *. (One nrs a') = One nrs (a * a')
  a *. (VVMatrix nrs ncs rs) =
    VVMatrix nrs ncs $ V.map (V.map (a*)) rs

instance Semiring a => MultiplicativeSemigroupRightAction a (VVMatrix a) where
  (Zero nrs ncs) .* a' = Zero nrs ncs
  (One nrs a) .* a' = One nrs (a * a')
  (VVMatrix nrs ncs rs) .* a' =
    VVMatrix nrs ncs $ V.map (V.map (*a')) rs

instance Rng a => LinearSemiringLeftAction a (VVMatrix a)
instance Rng a => LinearSemiringRightAction a (VVMatrix a)

instance Ring a => MultiplicativeLeftAction a (VVMatrix a)
instance Ring a => MultiplicativeRightAction a (VVMatrix a)

instance Ring a => LeftModule a (VVMatrix a)
instance Ring a => RightModule a (VVMatrix a)
instance (Commutative a, Ring a) => Module a (VVMatrix a)

instance Ring a => LeftAlgebra a (VVMatrix a)
instance Ring a => RightAlgebra a (VVMatrix a)
instance (Commutative a, Ring a) => Algebra a (VVMatrix a)


