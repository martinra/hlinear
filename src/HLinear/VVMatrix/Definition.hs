{-# LANGUAGE
    DataKinds
  , KindSignatures
  #-}

module HLinear.VVMatrix.Definition
where

import Data.Vector ( Vector )
import GHC.TypeLits ( Nat )
import Math.Structure
import Numeric.Natural ( Natural )


-- | VVMatrix models the set of matrices of all sizes with entries in a field.
--   Zero and One elements, which don't have distinguished size, are separate
--   constructors
data VVMatrix a =
    Zero !(Maybe Natural) !(Maybe Natural)
  | One !(Maybe Natural) a
  | VVMatrix !Natural !Natural (Vector (Vector a))

newtype SizedVVMatrix (m :: Nat) (n :: Nat) a = SizedVVMatrix (VVMatrix a)
