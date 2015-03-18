{-# LANGUAGE 
    DataKinds
  , KindSignatures 
  #-}

module HLinear.VVMatrix.Definition
where

import Data.Vector ( Vector )
import GHC.TypeLits ( Nat )


-- | VVMatrix models the union of matrices of all sizes with entries in a
--   Zero and One elements, which don't have distinguished size are separate
--   constructurs
data VVMatrix a =
    Zero
  | One
  | VVMatrix !Int !Int (Vector (Vector a))

newtype SizedVVMatrix (m :: Nat) (n :: Nat) a = SizedVVMatrix (VVMatrix a)
