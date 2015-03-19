module HLinear.VVMatrix.Algebra
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import Data.Composition ( (.:) )
import Data.Maybe ( fromJust )
import qualified Data.Vector as V
import Math.Structure

import HLinear.VVMatrix.Definition
import HLinear.VVMatrix.Basic 


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

instance ( AdditiveMonoid a, Abelean a ) => Abelean (VVMatrix a)
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


instance    ( AdditiveMonoid a, MultiplicativeMagma a )
         => MultiplicativeMagma (VVMatrix a) where
  -- todo: assert these are square matrices and let everything else work by
  -- module structures
  (VVMatrix nrs ncs rs) * (VVMatrix nrs' ncs' rs')
    = case cmbDim ncs nrs' of
        Nothing -> error "incompatible dimensions"
        Just 0  -> Zero (Just nrs) (Just ncs')
        _       -> VVMatrix nrs ncs' $
                     ( `V.map` rs ) $ \r ->
                       V.foldr1' (V.zipWith (+)) $
                       V.zipWith (\a -> V.map (a*)) r rs'

  (Zero nrs ncs) * (Zero nrs' ncs') =
    fromJust $ cmbDimMMay ncs nrs' >> Just ( Zero nrs ncs' )
  (Zero nrs ncs) * (One nrs' _) =
    fromJust $ cmbDimMMay ncs nrs' >> Just ( Zero nrs nrs' )
  (One nrs _) * (Zero nrs' ncs') =
    fromJust $ cmbDimMMay nrs nrs' >> Just ( Zero nrs ncs' )
  (Zero nrs ncs) * (VVMatrix nrs' ncs' _) =
    fromJust $ cmbDimMay nrs' ncs >> Just ( Zero nrs (Just ncs') )
  (VVMatrix nrs ncs _) * (Zero nrs' ncs') =
    fromJust $ cmbDimMay ncs nrs' >> Just ( Zero (Just nrs) ncs' )

  (One nrs a) * (VVMatrix nrs' ncs' rs') =
    fromJust $ cmbDimMay nrs' nrs >>
    Just ( VVMatrix nrs' ncs' $ V.map (V.map (a*)) rs' )
  
  (VVMatrix nrs ncs rs) * (One nrs' a') =
    fromJust $ cmbDimMay ncs nrs' >>
    Just ( VVMatrix nrs ncs $ V.map (V.map (*a')) rs )

instance    ( AdditiveMonoid a, MultiplicativeSemigroup a )
         => MultiplicativeSemigroup (VVMatrix a)

instance    ( AdditiveMonoid a, MultiplicativeMonoid a )
         => MultiplicativeMonoid (VVMatrix a) where
  one = One Nothing one


instance ( AdditiveMonoid a, Distributive a ) => Distributive (VVMatrix a)
instance ( AdditiveMonoid a, Semiring a ) => Semiring (VVMatrix a)
instance ( AdditiveMonoid a, Rig a ) => Rig (VVMatrix a)
instance ( AdditiveMonoid a, Rng a ) => Rng (VVMatrix a)
instance ( AdditiveMonoid a, Ring a ) => Ring (VVMatrix a)
