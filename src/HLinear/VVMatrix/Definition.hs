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


-- | VVMatrix models the union of matrices of all sizes with entries in a
--   Zero and One elements, which don't have distinguished size are separate
--   constructurs
data VVMatrix a =
-- todo: Int should be Natural
    Zero !(Maybe Natural) !(Maybe Natural)
  | One !(Maybe Natural) a
  | VVMatrix !Natural !Natural (Vector (Vector a))

newtype SizedVVMatrix (nrs :: Nat) (ncs :: Nat) a =
  SizedVVMatrix { fromSized :: (VVMatrix a) }
