{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  #-}

module HLinear.Matrix.MaybeIO
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Applicative ( liftA, liftA2 )
import Control.Exception ( catch, handle, SomeException )
import Control.Monad.Trans.Maybe
import Data.Vector ( Vector )
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
type MaybeIO = MaybeT IO


someExceptionToNothing :: SomeException -> IO (Maybe a)
someExceptionToNothing e = return Nothing

liftMaybeIO :: (forall a . sa a -> MaybeIO (ta a))
            -> (forall c . MaybeIO (tc c) -> sc c)
            -> (ta a -> tc c)
            -> sa a -> sc c
liftMaybeIO sta tsc f a = 
  let MaybeT fa = liftA f (sta a)
  in tsc $ MaybeT $ catch fa someExceptionToNothing

liftMaybeIO2 :: (forall a . sa a -> MaybeIO (ta a))
            -> (forall b . sb b -> MaybeIO (tb b))
            -> (forall c . MaybeIO (tc c) -> sc c)
            -> (ta a -> tb b -> tc c)
            -> sa a -> sb b -> sc c
liftMaybeIO2 sta stb tsc f a b =
  let MaybeT fab = liftA2 f (sta a) (stb b)
  in tsc $ MaybeT $ catch fab someExceptionToNothing

-- definition of matrices

newtype MaybeIOMatrix a = MaybeIOMatrix {toMatrix :: MaybeIO (Matrix a)}

liftMat :: (Matrix a -> Matrix b)
         -> MaybeIOMatrix a
         -> MaybeIOMatrix b
liftMat = liftMaybeIO toMatrix MaybeIOMatrix

liftMat2 :: (Matrix a -> Matrix b -> Matrix c)
         -> MaybeIOMatrix a -> MaybeIOMatrix b
         -> MaybeIOMatrix c
liftMat2 = liftMaybeIO2 toMatrix toMatrix MaybeIOMatrix

-- eq and show

instance Eq a => Eq (MaybeIOMatrix a) where
  m == m' = unsafePerformIO $ do
    em <- runMaybeT $ toMatrix m
    em' <- runMaybeT $ toMatrix m'
    return $ case (em, em') of
        (Nothing, Nothing)    -> True
        (Just rm, Just rm')   -> rm == rm'
        _                     -> False
--      (Left _, Left _)      -> True
--      (Right rm, Right rm') -> rm == rm'
--      _                     -> False

instance Show a => Show (MaybeIOMatrix a) where
  show m = unsafePerformIO $ do
    em <- runMaybeT $ toMatrix m
    return $ case em of
      Nothing -> "MaybeIOMatrix: Nothing"
      Just m  -> show m
--      Left s  -> "MaybeIOMatrix: " ++ s
--      Right m -> show m

-- construction from lists

fromLists :: [[a]] -> MaybeIOMatrix a
fromLists = MaybeIOMatrix . MaybeT .
            return . either (const Nothing) Just .
            M.fromLists

-- additive structure

instance AdditiveMagma a => AdditiveMagma (MaybeIOMatrix a) where
  (+) = liftMat2 (+)

instance AdditiveSemigroup a => AdditiveSemigroup (MaybeIOMatrix a)

instance Abelian a => Abelian (MaybeIOMatrix a)

-- action of base ring

instance    MultiplicativeSemigroup a
         => MultiplicativeSemigroupLeftAction a (MaybeIOMatrix a)
  where
  a *. m = liftMat (a*.) m

instance    MultiplicativeMonoid a
         => MultiplicativeLeftAction a (MaybeIOMatrix a)

instance Semiring a => LinearSemiringLeftAction a (MaybeIOMatrix a)


instance    MultiplicativeSemigroup a
         => MultiplicativeSemigroupRightAction a (MaybeIOMatrix a)
  where
  m .* a = liftMat (.*a) m

instance    MultiplicativeMonoid a
         => MultiplicativeRightAction a (MaybeIOMatrix a)

instance Semiring a => LinearSemiringRightAction a (MaybeIOMatrix a)

-- action on column vectors

newtype MaybeIOColumn a = MaybeIOColumn {toColumn :: MaybeIO (Column a)}

liftMatCol :: (Matrix a -> Column b -> Column c)
           -> MaybeIOMatrix a -> MaybeIOColumn b
           -> MaybeIOColumn c
liftMatCol = liftMaybeIO2 toMatrix toColumn MaybeIOColumn

instance    ( Rng a, AdditiveMonoid b
            , LinearSemiringLeftAction a b )
         => MultiplicativeSemigroupLeftAction
              (MaybeIOMatrix a) (MaybeIOColumn b)
  where
  (*.) = liftMatCol (*.)

newtype MaybeIOVector a = MaybeIOVector {toVector :: MaybeIO (Vector a)}

liftMatVec :: (Matrix a -> Vector b -> Vector c)
           -> MaybeIOMatrix a -> MaybeIOVector b
           -> MaybeIOVector c
liftMatVec = liftMaybeIO2 toMatrix toVector MaybeIOVector

instance Rng a => MultiplicativeSemigroupLeftAction
                    (MaybeIOMatrix a) (MaybeIOVector a)
  where
  (*.) = liftMatVec (*.)

-- multiplicative structure

instance Rng a => MultiplicativeMagma (MaybeIOMatrix a) where
  (*) = liftMat2 (*)

instance Rng a => MultiplicativeSemigroup (MaybeIOMatrix a)

-- algebra structure

instance Rng a => Distributive (MaybeIOMatrix a)
instance Rng a => Semiring (MaybeIOMatrix a)

instance ( Rng a, Commutative a ) => SemiLeftAlgebra a (MaybeIOMatrix a)
instance ( Rng a, Commutative a ) => SemiRightAlgebra a (MaybeIOMatrix a)
instance ( Rng a, Commutative a ) => SemiAlgebra a (MaybeIOMatrix a)

-- QuickCheck

instance Arbitrary a => Arbitrary (MaybeIOMatrix a) where
  arbitrary = (MaybeIOMatrix . return) <$> arbitrary

  shrink = map MaybeIOMatrix . invSequence . liftA shrink . toMatrix
    where
    invSequence :: MaybeIO [a] -> [MaybeIO a] 
    invSequence l = unsafePerformIO $ do
      el <- runMaybeT l
      return $ case el of
        Nothing -> []
        Just l' -> map return l'
--        Left _   -> []
--        Right l' -> map return l'
     
instance (Monad m, Serial m a) => Serial m (MaybeIOMatrix a) where
  series = (MaybeIOMatrix . return) <$> series



