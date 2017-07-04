{-# LANGUAGE
    ScopedTypeVariables
  , TemplateHaskell
  , UndecidableInstances
  #-}

module HLinear.Matrix.Algebra
where

import HLinear.Utility.Prelude

import Control.Applicative ( liftA, liftA2 )
import Control.Monad.Reader
import Data.Reflection
import qualified Data.Vector as V
import Language.Haskell.TH hiding ( reify )

import HFlint.NF ( NF, ReifiesNFContext )
import HFlint.FMPQ ( FMPQ )
import HFlint.FMPZ ( FMPZ )

import HLinear.Matrix.Block as M ( concatMap )
import HLinear.Matrix.Column
import HLinear.Matrix.Definition
import HLinear.Matrix.TH

--------------------------------------------------------------------------------
-- additive structure
--------------------------------------------------------------------------------

instance AdditiveMagma a => AdditiveMagma (Matrix a) where
  (Matrix nrs ncs rs) + (Matrix nrs' ncs' rs')
    | nrs /= nrs' || ncs /= ncs' = error "Matrix:+ incompatible dimensions"
    | otherwise =
      Matrix nrs ncs $ V.zipWith (V.zipWith (+)) rs rs'

instance Abelian a => Abelian (Matrix a)

instance AdditiveSemigroup a => AdditiveSemigroup (Matrix a)

instance AdditiveMonoid a => AdditiveMonoid (Matrix a) where
  zero = error "Matrix.zero would require size"

instance DecidableZero a => DecidableZero (Matrix a) where
  isZero = all isZero

instance AdditiveGroup a => AdditiveGroup (Matrix a) where
  negate = fmap negate

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
      (`fmap` rs) $ \r -> V.foldl1' (+) $ V.zipWith (*.) r v
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
type ReifiesNmbRows ctx = Reifies ctx Natural 

withRowLength
  :: Natural
  -> (forall ctx. ReifiesNmbRows ctx => Proxy ctx -> a)
  -> a
withRowLength = reify 


instance AdditiveMagma a => AdditiveMagma (Row ctx a) where
    (Row r) + (Row r') = Row $ V.zipWith (+) r r'

instance Abelian a => Abelian (Row ctx a)

instance AdditiveSemigroup a => AdditiveSemigroup (Row ctx a)


instance ( ReifiesNmbRows ctx, AdditiveMonoid a )
      => AdditiveMonoid (Row ctx a)
  where
  zero = zeroRow
    where
      zeroRow :: Row ctx a
      zeroRow = Row $ V.replicate (fromIntegral $ reflect (Proxy::Proxy ctx)) zero

instance ( ReifiesNmbRows ctx, DecidableZero a )
      => DecidableZero (Row ctx a)
  where
  isZero = V.all isZero . fromRow

instance Semiring a => MultiplicativeSemigroupLeftAction a (Row ctx a) where
  (*.) a = Row . fmap (a*) . fromRow

instance Semiring a => LinearSemiringLeftAction a (Row ctx a)

--------------------------------------------------------------------------------
-- multiplicative structure
--------------------------------------------------------------------------------

instance Rng a => MultiplicativeMagma (Matrix a) where
  m@(Matrix nrs ncs rs) * (Matrix nrs' ncs' rs')
    | ncs /= nrs' = error "Matrix * Matrix: incompatible dimensions"
    | otherwise = Matrix nrs ncs' $ withRowLength ncs' go
        where
          go :: forall ctx. ReifiesNmbRows ctx
             => Proxy ctx -> Vector (Vector a)
          go _ = fmap fromRow $ fromColumn $
                   m *. (Column $ fmap Row rs' :: Column (Row ctx a))

instance Rng a => MultiplicativeSemigroup (Matrix a)

instance Ring a => MultiplicativeMonoid (Matrix a) where
  one = error "Matrix.zero would require size"

instance ( Ring a, DecidableZero a, DecidableOne a ) => DecidableOne (Matrix a) where
  isOne (Matrix nrs ncs rs)
    | nrs /= ncs = False
    | otherwise = and $ (`V.imap` rs) $ \ix r ->
                    and $ (`V.imap` r) $ \jx ->
                      if ix == jx then isOne else isZero

tensorProduct :: Semiring a => Matrix a -> Matrix a -> Matrix a
tensorProduct m = M.concatMap (\a -> fmap (*a) m)

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
