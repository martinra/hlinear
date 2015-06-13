{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  #-}

module HLinear.Matrix.Algebra
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Applicative ( liftA, liftA2 )
import Control.Monad
import Control.Monad.Reader
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural

import HLinear.Matrix.Definition


-- additive structure

instance AdditiveMagma a => AdditiveMagma (Matrix a) where
  (Matrix nrs ncs rs) + (Matrix nrs' ncs' rs')
    | nrs /= nrs' || ncs /= ncs' = error "Matrix:+ incompatible dimensions"
    | otherwise =
      Matrix nrs ncs $ V.zipWith (V.zipWith (+)) rs rs'

instance AdditiveSemigroup a => AdditiveSemigroup (Matrix a)

instance Abelian a => Abelian (Matrix a)

-- action of base ring

instance    MultiplicativeSemigroup a
         => MultiplicativeSemigroupLeftAction a (Matrix a)
  where
  a *. (Matrix nrs ncs rs) = Matrix nrs ncs $ V.map (V.map (a*)) rs

instance    MultiplicativeMonoid a
         => MultiplicativeLeftAction a (Matrix a)

instance Semiring a => LinearSemiringLeftAction a (Matrix a)


instance    MultiplicativeSemigroup a
         => MultiplicativeSemigroupRightAction a (Matrix a)
  where
  (Matrix nrs ncs rs) .* a = Matrix nrs ncs $ V.map (V.map (*a)) rs

instance    MultiplicativeMonoid a
         => MultiplicativeRightAction a (Matrix a)

instance Semiring a => LinearSemiringRightAction a (Matrix a)

-- action on column vectors

newtype Column a = Column {unColumn :: Vector a}

instance    ( Rng a, AdditiveMonoid b
            , LinearSemiringLeftAction a b )
         => MultiplicativeSemigroupLeftAction (Matrix a) (Column b)
  where
  (Matrix nrs ncs rs) *. Column v
    | ncs /= nv = error $ "Matrix *. Column: incompatible dimensions"
    | ncs == 0 = Column $ V.replicate nrsZ zero
    | otherwise = Column $ 
      (`V.map` rs) $ \r -> V.foldl1' (+) $ V.zipWith (*.) r v
    where
      nv = fromIntegral $ V.length v
      nrsZ = fromIntegral nrs

instance Rng a => MultiplicativeSemigroupLeftAction (Matrix a) (Vector a)
  where
  m *. v = unColumn $ (m *.) $ Column v

-- rows of matrices (with given length)

-- to define zero of rows we use the reader monoad to ensure dimension
-- restrictions
newtype MRow a = MRow {unMRow :: Reader Natural a}

instance Functor MRow where
  fmap f = MRow . fmap f . unMRow

instance Applicative MRow where
  pure = return
  (<*>) = ap
  
instance Monad MRow where
  return = MRow . return
  m >>= k = MRow $ unMRow m >>= unMRow . k


instance AdditiveMagma a => AdditiveMagma (MRow (Vector a)) where
    (+)= liftA2 $ V.zipWith (+)

instance AdditiveSemigroup a => AdditiveSemigroup (MRow (Vector a))

instance AdditiveMonoid a => AdditiveMonoid (MRow (Vector a))
  where
  zero = do
    n <- MRow $ fromIntegral <$> ask
    return $ V.replicate n zero

instance DecidableZero a => DecidableZero (MRow (Vector a))
  where
  -- well-definedness of this depends havily on the semantics of the Reader
  -- argument
  isZero = runReader undefined . liftA (V.map isZero) . unMRow

instance Abelian a => Abelian (MRow (Vector a))


instance    Semiring a
         => MultiplicativeSemigroupLeftAction
              a (MRow (Vector a))
  where
  a *. r = V.map (a*) <$> r

instance Semiring a => LinearSemiringLeftAction a (MRow (Vector a))

-- multiplicative structure

instance Rng a => MultiplicativeMagma (Matrix a) where
  m@(Matrix nrs ncs rs) * (Matrix nrs' ncs' rs')
    | ncs /= nrs' = error "Matrix * Matrix: incompatible dimensions"
    | otherwise = Matrix nrs ncs' $ flip runReader ncs' $ unMRow $
                    V.sequence $ unColumn $ m *. (Column $ V.map return rs')

instance Rng a => MultiplicativeSemigroup (Matrix a)

-- algebra structure

instance Rng a => Distributive (Matrix a)
instance Rng a => Semiring (Matrix a)

instance ( Rng a, Commutative a ) => SemiLeftAlgebra a (Matrix a)
instance ( Rng a, Commutative a ) => SemiRightAlgebra a (Matrix a)
instance ( Rng a, Commutative a ) => SemiAlgebra a (Matrix a)
