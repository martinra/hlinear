module VVMatrixTests.Utils
where

import Data.Vector ( Vector )
import HLinear.VVMatrix
import qualified TestHLinear.Utils as U


-- We need to specify the type, so that a is not specialized when infering the
-- type on first occurence of equal and equal2
equal  :: Eq a
       => (VVMatrix Rational -> a)
       -> (Vector (Vector Rational) -> a)
       -> VVMatrix Rational
       -> Bool
equal2 :: Eq a
       => (VVMatrix Rational -> VVMatrix Rational -> a)
       -> (Vector (Vector Rational) -> Vector (Vector Rational) -> a)
       -> VVMatrix Rational -> VVMatrix Rational
       -> Bool
intertwining ::
         (VVMatrix Rational -> VVMatrix Rational)
      -> (Vector (Vector Rational) -> Vector (Vector Rational))
      -> VVMatrix Rational
      -> Bool
intertwining2 ::
         (VVMatrix Rational -> VVMatrix Rational -> VVMatrix Rational )
      -> (Vector (Vector Rational) -> Vector (Vector Rational) -> Vector (Vector Rational))
      -> VVMatrix Rational -> VVMatrix Rational
      -> Bool
equal         = U.equal toVectors fromVectors
equal2        = U.equal2 toVectors fromVectors
intertwining  = U.intertwining toVectors fromVectors
intertwining2 = U.intertwining2 toVectors fromVectors


