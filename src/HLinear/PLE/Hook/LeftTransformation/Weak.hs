{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , StandaloneDeriving
  #-}

module HLinear.PLE.Hook.LeftTransformation.Weak
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      , head, tail
                      )

import Control.DeepSeq ( NFData(..) )
import Data.Proxy
import Data.Reflection
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural

import HLinear.PLE.Hook.LeftTransformation.Definition
 ( LeftTransformation(LeftTransformation) )
import HLinear.PLE.Hook.LeftTransformation.Column
 ( LeftTransformationColumn(LeftTransformationColumn) )


 -- \ A vector of columns (a, [v]) which are offset by their index.
 --   It represents a transformation from the left
 --     a1     0     0   0
 --   v*a1    a2     0   0
 --   v*a1  v*a2    a3   0 
 --   v*a1  v*a2  v*a3  a4
 --   . . . .

data WeakLeftTransformation a =
  WeakLeftTransformation Natural (Vector (WeakLeftTransformationColumn a))

instance NFData a => NFData (WeakLeftTransformation a) where
  rnf (WeakLeftTransformation nrs cs) =
    seq (rnf nrs) $ seq (rnf cs) ()

nmbRows :: WeakLeftTransformation a -> Natural
nmbRows (WeakLeftTransformation nrs _) = nrs

columns :: WeakLeftTransformation a -> Vector (WeakLeftTransformationColumn a)
columns (WeakLeftTransformation _ cs) = cs

(!) :: WeakLeftTransformation a -> Int -> WeakLeftTransformationColumn a
(!) (WeakLeftTransformation _ cs) jx = cs V.! jx

-- columns

data WeakLeftTransformationColumn a =
  WeakLeftTransformationColumn Int a (Vector a)


offset :: WeakLeftTransformationColumn a -> Int
offset (WeakLeftTransformationColumn o _ _) = o

head :: WeakLeftTransformationColumn a -> a
head (WeakLeftTransformationColumn _ a _) = a

tail :: WeakLeftTransformationColumn a -> Vector a
tail (WeakLeftTransformationColumn _ _ as) = as


fromLeftTransformationColumn
  :: LeftTransformationColumn a -> WeakLeftTransformationColumn a
fromLeftTransformationColumn (LeftTransformationColumn o a v) =
  WeakLeftTransformationColumn o (fromNonZero a) v

deriving instance Show a => Show (WeakLeftTransformationColumn a)

instance Eq a => Eq (WeakLeftTransformationColumn a) where
  (WeakLeftTransformationColumn s a v) == (WeakLeftTransformationColumn s' a' v') =
    s == s' && a == a' && (`V.all` V.zip v v') (uncurry (==))

instance NFData a => NFData (WeakLeftTransformationColumn a) where
  rnf (WeakLeftTransformationColumn s a c) =
    seq (rnf s) $ seq (rnf a) $ seq (rnf c) ()

-- conversion

fromLeftTransformation
  :: LeftTransformation a -> WeakLeftTransformation a
fromLeftTransformation (LeftTransformation nrs cs) =
  WeakLeftTransformation nrs $ V.map fromLeftTransformationColumn cs


instance Functor WeakLeftTransformation where
  fmap f (WeakLeftTransformation nrs cs) =
    WeakLeftTransformation nrs $ V.map (fmap f) cs

instance Functor WeakLeftTransformationColumn where
  fmap f (WeakLeftTransformationColumn o a as) =
    WeakLeftTransformationColumn o (f a) $ V.map f as

instance
     AdditiveMagma a
  => AdditiveMagma (WeakLeftTransformation a)
  where
  lt@(WeakLeftTransformation nrs cs) + lt'@(WeakLeftTransformation nrs' cs')
    | nrs /= nrs' = error "incompatible dimensions"
    | otherwise =
        WeakLeftTransformation nrs cs''
        where
          cs'' = V.zipWith addColumn cs cs' V.++ back
          back = case compare ncsZ ncs'Z of
                   EQ -> V.empty
                   GT -> V.drop ncs'Z cs 
                   LT -> V.drop ncsZ cs'
          addColumn (WeakLeftTransformationColumn o a as)
                    (WeakLeftTransformationColumn o' a' as') =
            WeakLeftTransformationColumn o (a+a') $ V.zipWith (+) as as'

          ncsZ = V.length cs
          ncs'Z = V.length cs'


instance
     AdditiveSemigroup a
  => AdditiveSemigroup (WeakLeftTransformation a)

instance Abelian a => Abelian (WeakLeftTransformation a)

instance
     MultiplicativeSemigroupLeftAction a b
  => MultiplicativeSemigroupLeftAction a (WeakLeftTransformationColumn b)
  where
    a *. (WeakLeftTransformationColumn o b bs) =
      WeakLeftTransformationColumn o (a*.b) $ V.map (a*.) bs
  
instance
     MultiplicativeSemigroupLeftAction a b
  => MultiplicativeSemigroupLeftAction a (WeakLeftTransformation b)
  where
    a *. (WeakLeftTransformation ncs cs) =
      WeakLeftTransformation ncs $ V.map (a*.) cs

instance
     ( LinearSemiringLeftAction a b, AdditiveSemigroup b )
  => LinearSemiringLeftAction a (WeakLeftTransformation b)
