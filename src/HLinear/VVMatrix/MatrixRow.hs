{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.VVMatrix.MatrixRow
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure

import HLinear.VVMatrix.Creation ( toVectors )
import HLinear.VVMatrix.Definition ( VVMatrix )


newtype MatrixRow a = MatrixRow {unMatrixRow :: Vector a}

rows :: AdditiveMonoid a
     => VVMatrix a -> Maybe (Vector (MatrixRow a))
rows m = V.map MatrixRow <$> toVectors m


instance AdditiveMagma a => AdditiveMagma (MatrixRow a) where
  MatrixRow a + MatrixRow b = MatrixRow $
    V.zipWith (+) a b
    V.++
    case compare na nb of
      LT -> V.drop na b
      EQ -> V.empty
      GT -> V.drop nb a
    where
    na = V.length a
    nb = V.length b
          
instance AdditiveSemigroup a => AdditiveSemigroup (MatrixRow a)
instance AdditiveMonoid a => AdditiveMonoid (MatrixRow a) where
  zero = MatrixRow V.empty
instance Abelian a => Abelian (MatrixRow a)

instance AdditiveGroup a => AdditiveGroup (MatrixRow a) where
  negate (MatrixRow v) = MatrixRow $ V.map negate v

instance    MultiplicativeSemigroup a
         => MultiplicativeSemigroupLeftAction a (MatrixRow a) where
  a *. (MatrixRow v) = MatrixRow $ V.map (a*) v

instance MultiplicativeMonoid a => MultiplicativeLeftAction a (MatrixRow a)
instance Ring a => LinearSemiringLeftAction a (MatrixRow a)
instance Ring a => LeftModule a (MatrixRow a)
