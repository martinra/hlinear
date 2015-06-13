{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  #-}

module HLinear.Matrix.Maybe
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Arrow ( (***) )
import Control.Monad ( guard )
import Control.Applicative ( liftA, liftA2 )
import Control.Exception ( catch, handle, SomeException )
import Control.Monad.Trans.Maybe
import Data.Composition ( (.:) )
import Data.Maybe
import Data.Vector ( Vector )
import qualified Data.Vector as V
import System.IO
import Math.Structure
import System.IO.Unsafe ( unsafePerformIO )
import Test.QuickCheck.Arbitrary ( Arbitrary, arbitrary, shrink )
import Test.SmallCheck.Series ( Serial, series )

import qualified HLinear.Matrix as M
import HLinear.Matrix.Algebra ( Column(..) )
import HLinear.Matrix.Basic
import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.Matrix.QuickCheck
import HLinear.Matrix.SmallCheck


-- wrap the paritally defined matrices to obtain completely defined objects

-- liftMaybeIO :: (forall a . sa a -> Maybe (ta a))
--             -> (forall c . Maybe (tc c) -> sc c)
--             -> (ta a -> tc c)
--             -> sa a -> sc c
-- liftMaybeIO sta tsc f a = 
--   let MaybeT fa = liftA f (sta a)
--   in tsc $ MaybeT $ catch fa someExceptionToNothing
-- 
-- liftMaybeIO2 :: (forall a . sa a -> Maybe (ta a))
--             -> (forall b . sb b -> Maybe (tb b))
--             -> (forall c . Maybe (tc c) -> sc c)
--             -> (ta a -> tb b -> tc c)
--             -> sa a -> sb b -> sc c
-- liftMaybeIO2 sta stb tsc f a b =
--   let MaybeT fab = liftA2 f (sta a) (stb b)
--   in tsc $ MaybeT $ catch fab someExceptionToNothing

-- definition of matrices

newtype MaybeMatrix a = MaybeMatrix {fromMaybeMatrix :: Maybe (Matrix a)}

liftMat :: (Matrix a -> Matrix b)
         -> MaybeMatrix a
         -> MaybeMatrix b
liftMat f = MaybeMatrix . liftA f . fromMaybeMatrix

-- eq and show

instance Eq a => Eq (MaybeMatrix a) where
  m == m' = case (fromMaybeMatrix m, fromMaybeMatrix m') of
              (Nothing, Nothing)  -> True
              (Just rm, Just rm') -> rm == rm'
              _                   -> False

instance Show a => Show (MaybeMatrix a) where
  show m = case fromMaybeMatrix m of
             Nothing -> "MaybeMatrix: Nothing"
             Just m  -> show m

-- construction from lists

fromLists :: [[a]] -> MaybeMatrix a
fromLists = MaybeMatrix . either (const Nothing) Just . M.fromLists

-- additive structure

instance AdditiveMagma a => AdditiveMagma (MaybeMatrix a) where
  (MaybeMatrix m) + (MaybeMatrix m') = MaybeMatrix $ do
    guard $ liftA nmbRows m == liftA nmbRows m'
    guard $ liftA nmbCols m == liftA nmbCols m'
    liftA2 (+) m m'

instance AdditiveSemigroup a => AdditiveSemigroup (MaybeMatrix a)

instance Abelian a => Abelian (MaybeMatrix a)

-- action of base ring

instance    MultiplicativeSemigroup a
         => MultiplicativeSemigroupLeftAction a (MaybeMatrix a)
  where
  a *. m = liftMat (a*.) m

instance    MultiplicativeMonoid a
         => MultiplicativeLeftAction a (MaybeMatrix a)

instance Semiring a => LinearSemiringLeftAction a (MaybeMatrix a)


instance    MultiplicativeSemigroup a
         => MultiplicativeSemigroupRightAction a (MaybeMatrix a)
  where
  m .* a = liftMat (.*a) m

instance    MultiplicativeMonoid a
         => MultiplicativeRightAction a (MaybeMatrix a)

instance Semiring a => LinearSemiringRightAction a (MaybeMatrix a)

-- action on column vectors

newtype MaybeColumn a = MaybeColumn {fromMaybeColumn :: Maybe (Column a)}

instance    ( Rng a, AdditiveMonoid b
            , LinearSemiringLeftAction a b )
         => MultiplicativeSemigroupLeftAction
              (MaybeMatrix a) (MaybeColumn b)
  where
  (MaybeMatrix m) *. (MaybeColumn c) = MaybeColumn $ do
    guard $ liftA nmbCols m == liftA (fromIntegral . V.length . unColumn) c
    liftA2 (*.) m c

newtype MaybeVector a = MaybeVector {fromMaybeVector :: Maybe (Vector a)}

instance Rng a => MultiplicativeSemigroupLeftAction
                    (MaybeMatrix a) (MaybeVector a)
  where
  (MaybeMatrix m) *. (MaybeVector v) = MaybeVector $ do
     guard $ liftA nmbCols m == liftA (fromIntegral . V.length ) v
     liftA2 (*.) m v

-- multiplicative structure

instance Rng a => MultiplicativeMagma (MaybeMatrix a) where
  (MaybeMatrix m) * (MaybeMatrix m') = MaybeMatrix $ do
    guard $ liftA nmbCols m == liftA nmbRows m'
    liftA2 (*) m m'

instance Rng a => MultiplicativeSemigroup (MaybeMatrix a)

-- algebra structure

instance Rng a => Distributive (MaybeMatrix a)
instance Rng a => Semiring (MaybeMatrix a)

instance ( Rng a, Commutative a ) => SemiLeftAlgebra a (MaybeMatrix a)
instance ( Rng a, Commutative a ) => SemiRightAlgebra a (MaybeMatrix a)
instance ( Rng a, Commutative a ) => SemiAlgebra a (MaybeMatrix a)

-- QuickCheck

instance Arbitrary a => Arbitrary (MaybeMatrix a) where
  arbitrary = (MaybeMatrix . return) <$> arbitrary

  shrink = map (MaybeMatrix . Just) . fromMaybe [] .
           liftA shrink . fromMaybeMatrix
     
instance (Monad m, Serial m a) => Serial m (MaybeMatrix a) where
  series = (MaybeMatrix . return) <$> series
