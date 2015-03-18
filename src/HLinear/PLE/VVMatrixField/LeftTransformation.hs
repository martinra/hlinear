{-# LANGUAGE
    ScopedTypeVariables
  #-}

module HLinear.PLE.VVMatrixField.LeftTransformation
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Vector ( Vector(..), (!) )
import qualified Data.Vector as V
import Math.Structure

import HLinear.VVMatrix.Definition ( VVMatrix(..) )


 -- \ A vector of columns (a1^-1, [-ai]) which are offset by their index.
 --   Writing a for the first entry and v for entries of the column vector,
 --   it represents the matrix of the form
 --   a^-1 0    0    0
 --   -v   a^-1 0    0
 --   -v   -v   a^-1 0 
 --   -v   -v   -v   a^-1
 --   . . . .
data LeftTransformation a =
  LeftTransformation Int Int (Vector (NonZero a, Vector a))

toVVMatrix :: forall a . Field a
           => LeftTransformation a -> VVMatrix a
toVVMatrix (LeftTransformation nrs ncs cs) = 
  VVMatrix nrs ncs $ V.generate nrs row
  where
  row :: Field a => Int -> Vector a
  row i = V.generate ncs $ \j -> case compare i j of
            LT -> zero
            EQ -> fromNonZero $ recip  $ fst $ cs ! j
            GT -> negate $ (snd $ cs ! j) ! (i-j-1)


nmbRows :: LeftTransformation a -> Int
nmbRows (LeftTransformation nrs _ _) = nrs

nmbCols :: LeftTransformation a -> Int
nmbCols (LeftTransformation _ ncs _) = ncs

concat :: LeftTransformation a -> LeftTransformation a
       -> LeftTransformation a
concat (LeftTransformation nrs ncs cs)
       (LeftTransformation nrs' ncs' cs') =
  LeftTransformation nrs (ncs+ncs') $ cs V.++ cs'

apply :: Field a
      => LeftTransformation a -> VVMatrix a
      -> VVMatrix a
apply (LeftTransformation _ ncs cs) (VVMatrix nrs ncs' rs) =
  VVMatrix nrs ncs' rsL
  where
  rsPivotRecip = V.zipWith ( \(pivotRecip,_) r
                             -> V.map (* (fromNonZero pivotRecip)) r )
                           cs rs
  rsL = V.generate nrs $ \i ->
        V.foldl' (V.zipWith (+))
        (if i < ncs then rsPivotRecip ! i else rs ! i) $
        V.zipWith (\c r -> V.map ((*) $ snd c ! (i-1)) r)
                  (V.take (i-1) cs) rsPivotRecip

