{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , Rank2Types
  , ScopedTypeVariables
  , StandaloneDeriving
  , TemplateHaskell
  , UndecidableInstances
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
import Data.Proxy
import Data.Reflection
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural
import Language.Haskell.TH hiding ( reify )

import HFlint.NF ( NF, ReifiesNFContext )
import HFlint.FMPQ ( FMPQ )
import HFlint.FMPZ ( FMPZ )

import HLinear.Matrix.Column
import HLinear.Matrix.Definition
import HLinear.Matrix.Invertible
import HLinear.Matrix.TH

--------------------------------------------------------------------------------
-- additive structure
--------------------------------------------------------------------------------

instance AdditiveMagma a => AdditiveMagma (Matrix a) where
  (Matrix nrs ncs rs) + (Matrix nrs' ncs' rs')
    | nrs /= nrs' || ncs /= ncs' = error "Matrix:+ incompatible dimensions"
    | otherwise =
      Matrix nrs ncs $ V.zipWith (V.zipWith (+)) rs rs'

instance AdditiveSemigroup a => AdditiveSemigroup (Matrix a)

instance Abelian a => Abelian (Matrix a)

--------------------------------------------------------------------------------
-- action on columns
--------------------------------------------------------------------------------

instance    ( Rng a, AdditiveMonoid b
            , LinearSemiringLeftAction a b )
         => MultiplicativeSemigroupLeftAction (Matrix a) (Column b)
  where
  (Matrix nrs ncs rs) *. Column v
    | ncs /= nv = error "Matrix *. Column: incompatible dimensions"
    | ncs == 0 = Column $ V.replicate nrsZ zero
    | otherwise = Column $ 
      (`V.map` rs) $ \r -> V.foldl1' (+) $ V.zipWith (*.) r v
    where
      nv = fromIntegral $ V.length v
      nrsZ = fromIntegral nrs

instance Rng a => MultiplicativeSemigroupLeftAction (Matrix a) (Vector a)
  where
  m *. v = fromColumn $ (m *.) $ Column v

--------------------------------------------------------------------------------
-- rows of matrices (with given length)
--------------------------------------------------------------------------------

newtype Row ctx a = Row { fromRow :: Vector a }

withRowLength
  :: Natural
  -> (forall ctx. Reifies ctx Natural => Proxy ctx -> a)
  -> a
withRowLength = reify 


instance AdditiveMagma a => AdditiveMagma (Row ctx a) where
    (Row r) + (Row r') = Row $ V.zipWith (+) r r'

instance Abelian a => Abelian (Row ctx a)

instance AdditiveSemigroup a => AdditiveSemigroup (Row ctx a)


instance ( Reifies ctx Natural, AdditiveMonoid a )
      => AdditiveMonoid (Row ctx a)
  where
  zero = zeroRow
    where
      zeroRow :: Row ctx a
      zeroRow = Row $ V.replicate (fromIntegral $ reflect (Proxy::Proxy ctx)) zero

instance ( Reifies ctx Natural, DecidableZero a )
      => DecidableZero (Row ctx a)
  where
  isZero = V.all isZero . fromRow

instance Semiring a => MultiplicativeSemigroupLeftAction a (Row ctx a) where
  (*.) a = Row . V.map (a*) . fromRow

instance Semiring a => LinearSemiringLeftAction a (Row ctx a)

--------------------------------------------------------------------------------
-- multiplicative structure
--------------------------------------------------------------------------------

instance Rng a => MultiplicativeMagma (Matrix a) where
  m@(Matrix nrs ncs rs) * (Matrix nrs' ncs' rs')
    | ncs /= nrs' = error "Matrix * Matrix: incompatible dimensions"
    | otherwise = Matrix nrs ncs' $ withRowLength ncs' go
        where
          go :: forall ctx. Reifies ctx Natural
             => Proxy ctx -> Vector (Vector a)
          go _ = V.map fromRow $ fromColumn $
                   m *. (Column $ V.map Row rs' :: Column (Row ctx a))

instance Rng a => MultiplicativeSemigroup (Matrix a)

--------------------------------------------------------------------------------
-- action of base ring
--------------------------------------------------------------------------------

baseRingAction (return []) [t|FMPZ|]
baseRingAction (return []) [t|FMPQ|]
do ctx <- newName "ctx"
   reifies <- [t|ReifiesNFContext|]
   nf <- [t|NF|]
   baseRingAction
     (return [AppT reifies (VarT ctx)])
     (return $ AppT nf (VarT ctx))
