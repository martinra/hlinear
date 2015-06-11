{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  #-}

module HLinear.BRMatrix.Algebra
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

-- import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure

import HLinear.BRMatrix.Basic
import HLinear.BRMatrix.Definition
import HLinear.BRMatrix.RVector ( RVector(..) )
import qualified HLinear.BRMatrix.RVector as RV


-- additive structure

instance AdditiveMagma a => AdditiveMagma (BRMatrix a) where
  (BRMatrix nrs ncs rs) + (BRMatrix nrs' ncs' rs') =
    BRMatrix (max nrs nrs') (max ncs ncs') $
    RV.lift2Overlap (\l r r' -> RVector $ l V.++ V.zipWith (+) r r') rs rs'

instance AdditiveSemigroup a => AdditiveSemigroup (BRMatrix a)

instance AdditiveSemigroup a => AdditiveMonoid (BRMatrix a) where
  zero = BRMatrix 0 0 $ RVector V.empty

instance DecidableZero a => DecidableZero (BRMatrix a) where
  isZero (BRMatrix _ _ rs) = V.all (V.all isZero . toCurrentVector) $
                               toCurrentVector rs

instance AdditiveGroup a => AdditiveGroup (BRMatrix a) where
  negate (BRMatrix nrs ncs rs) =
    BRMatrix nrs ncs $ RV.liftRV (V.map $ RV.liftRV $ V.map negate) rs

instance Abelian a => Abelian (BRMatrix a)

-- multiplicative structure

newtype Column a = Column {unColumn :: RVector a}

instance    ( Rng a, AdditiveMonoid b
            , LinearSemiringLeftAction a b )
         => MultiplicativeSemigroupLeftAction (BRMatrix a) (Column b)
  where
  (BRMatrix nrs ncs rs) *. Column v = Column $ 
    ($rs) $ RV.liftRV $ V.map $ \r ->
      V.foldl' (+) zero $
      RV.lift2Discard (V.zipWith (*.)) r v

instance Rng a => MultiplicativeSemigroupLeftAction (BRMatrix a) (RVector a)
  where
  m *. v = unColumn $ (m *.) $ Column v


instance Rng a => MultiplicativeMagma (BRMatrix a) where
  m@(BRMatrix nrs ncs rs) * (BRMatrix nrs' ncs' rs') =
    BRMatrix nrs ncs' $ unColumn $ m *. Column rs'

instance Rng a => MultiplicativeSemigroup (BRMatrix a)
