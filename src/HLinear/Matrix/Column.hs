{-# LANGUAGE
    DeriveTraversable
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  #-}

module HLinear.Matrix.Column
where

import Data.Vector ( Vector )
import qualified Data.Vector as V

import Test.Vector ()
import Test.QuickCheck.Arbitrary ( Arbitrary )
import Test.SmallCheck.Series ( Serial, series )


newtype Column a = Column {fromColumn :: Vector a}
  deriving ( Eq, Show )


deriving instance Monoid (Column a)


deriving instance Functor Column

deriving instance Foldable Column

deriving instance Traversable Column

zipWith :: (a -> b -> c) -> Column a -> Column b -> Column c
zipWith f (Column v) (Column v') = Column $ V.zipWith f v v'


deriving instance Arbitrary a => Arbitrary (Column a)

instance Serial m a => Serial m (Column a)
  where
  series = Column <$> series
