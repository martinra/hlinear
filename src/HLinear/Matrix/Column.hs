{-# LANGUAGE
    DeriveTraversable
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  #-}

module HLinear.Matrix.Column
where

import Data.Vector ( Vector )
import qualified Data.Vector as V


newtype Column a = Column {fromColumn :: Vector a}


deriving instance Functor Column

deriving instance Foldable Column

deriving instance Traversable Column

zipWith :: (a -> b -> c) -> Column a -> Column b -> Column c
zipWith f (Column v) (Column v') = Column $ V.zipWith f v v'
