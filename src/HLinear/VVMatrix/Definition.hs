{-# LANGUAGE 
    DataKinds
  , KindSignatures 
  #-}

module HLinear.VVMatrix.Definition
where

import Data.Vector ( Vector )
import GHC.TypeLits ( Nat )
import Math.Structure


-- | VVMatrix models the union of matrices of all sizes with entries in a
--   Zero and One elements, which don't have distinguished size are separate
--   constructurs
data VVMatrix a =
-- todo: Int should be Natural
    Zero !(Maybe Int) !(Maybe Int)
  | One !(Maybe Int) a
  | VVMatrix !Int !Int (Vector (Vector a))

newtype SizedVVMatrix (m :: Nat) (n :: Nat) a = SizedVVMatrix (VVMatrix a)
