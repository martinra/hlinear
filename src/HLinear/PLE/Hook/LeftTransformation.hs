{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.PLE.Hook.LeftTransformation
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Applicative ( (<$>) )
import Control.Arrow ( first )
import Data.Maybe
import Data.Vector ( Vector(..) )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.PLE.Hook.ReversePermute
import HLinear.VVMatrix hiding ( (!), (!?) )
import HLinear.VVMatrix.Utils
import HLinear.VVMatrix.Definition ( VVMatrix(..) )

 -- \ A vector of columns (a, [v]) which are offset by their index.
 --   It represents a transformation from the left
 --     a1     0     0   0
 --   v*a1    a2     0   0
 --   v*a1  v*a2    a3   0 
 --   v*a1  v*a2  v*a3  a4
 --   . . . .

data LeftTransformation a =
  LeftTransformation Natural (Vector (LeftTransformationColumn a))
  deriving Show

nmbRows :: LeftTransformation a -> Natural
nmbRows (LeftTransformation nrs _) = nrs

nmbCols :: LeftTransformation a -> Natural
nmbCols (LeftTransformation _ cs) = fromIntegral $ V.length cs

ltDrop :: Int -> LeftTransformation a -> LeftTransformation a
ltDrop ix (LeftTransformation nrs cs) = LeftTransformation nrs' $ V.drop ix cs
  where
  nrs' = fromIntegral $ fromIntegral nrs - ix


data LeftTransformationColumn a =
  LeftTransformationColumn Int (NonZero a) (Vector a)
  deriving Show

(!) :: LeftTransformationColumn a -> Int -> a
(!) (LeftTransformationColumn offset a vs) ix
  | ix < offset  = error "LeftTransformationColumn (!) out of range"
  | ix == offset = fromNonZero a
  | otherwise    = vs V.! (ix - offset)


ltShiftOffset :: Int -> LeftTransformationColumn a
              -> LeftTransformationColumn a
ltShiftOffset shift (LeftTransformationColumn offset a vs) =
  LeftTransformationColumn (offset + shift) a vs

ltHead :: LeftTransformationColumn a -> a  
ltHead (LeftTransformationColumn _ a _) = fromNonZero a

ltHeadNonZero :: LeftTransformationColumn a -> NonZero a
ltHeadNonZero (LeftTransformationColumn _ a _) = a

ltHeadRecip :: MultiplicativeGroup (NonZero a)
            => LeftTransformationColumn a -> a  
ltHeadRecip (LeftTransformationColumn _ na _) = fromNonZero $ recip na

ltTail :: LeftTransformationColumn a -> Vector a
ltTail (LeftTransformationColumn _ _ v) = v



 -- | in the PLE decomopition a left transformation corresponds to the inverse
 --   of the matrix given above
 --    a1^-1         0           0      0
 --   -a2^-1*v   a2^-1           0      0
 --   -a3^-1*v  -a3^-1*v   a3^-1        0
 --   -a4^-1*v  -a4^-1*v  -a4^-1*v  a4^-1

toMatrix' :: ( Ring a, Ring b )
          => (LeftTransformationColumn a -> b)
          -> (b -> a)
          -> (b -> Int -> LeftTransformationColumn a -> a)
          -> LeftTransformation a -> VVMatrix a
toMatrix' fb fDiag fCol (LeftTransformation nrs cs) =
  VVMatrix nrs nrs $
    V.generate nrs' $ \ix ->
      let b = maybe one fb $ cs V.!? ix
      in V.generate nrs' $ \jx ->
        case compare ix jx of
          LT -> zero
          EQ -> fDiag b
          GT -> maybe zero (fCol b ix) $ cs V.!? jx
  where
  nrs' = fromIntegral nrs

toMatrix :: Ring a
         => LeftTransformation a -> VVMatrix a
toMatrix = toMatrix' ltHead id
           (\a ix -> (*a) . (!ix))

toInverseMatrix :: DivisionRing a
                => LeftTransformation a -> VVMatrix a
toInverseMatrix = toMatrix' ltHeadRecip id
                  (\arecip ix -> (arecip*) . negate . (!ix))


instance MultiplicativeSemigroupLeftAction ReversePermute (LeftTransformationColumn a) where
  p *. (LeftTransformationColumn s a v)
    | sizeRP p > V.length v = error "to large permutation"
    | otherwise              = LeftTransformationColumn s a (p *. v)

instance MultiplicativeSemigroupLeftAction ReversePermute (LeftTransformation a) where
  p *. (LeftTransformation nrs cs) = LeftTransformation nrs $ V.map (p*.) cs


instance DivisionRing a => MultiplicativeMagma (LeftTransformation a) where
  lt@(LeftTransformation nrs cs) * (LeftTransformation nrs' cs') =
    LeftTransformation (max nrs nrs') $
      csLeft V.++ V.map (LeftTransformation nrs' csMiddleRight *.) cs' V.++ csRight
    where
    ncs = V.length cs
    ncs' = V.length cs'
    nrsDiff = min 0 $ fromIntegral nrs - fromIntegral nrs'
    (csLeft, csMiddleRight) = V.splitAt nrsDiff cs
    (csMiddle, csRight) = V.splitAt ncs' csMiddleRight

instance DivisionRing a => MultiplicativeSemigroup (LeftTransformation a)
instance DivisionRing a => MultiplicativeMonoid (LeftTransformation a) where
  one = LeftTransformation 0 V.empty
instance    ( DivisionRing a, DecidableZero a, DecidableOne a )
         => DecidableOne (LeftTransformation a) where
  isOne (LeftTransformation nrs cs) =
    V.null cs
    ||
    (`all` cs) (\(LeftTransformationColumn _ a v) -> isOne (fromNonZero a) && all isZero v)

instance    (DivisionRing a, LeftModule a b)
         => MultiplicativeSemigroupLeftAction
              (LeftTransformation a) (Vector b)
  where
  (LeftTransformation nrs cs) *. v =
    V.foldr' go v $ V.drop (fromIntegral nrs - nv) cs
    where
    nv = length v
    -- this unsafely assumes that vn is longer than v'
    go c@(LeftTransformationColumn s' a' v') vn =
       V.init vn1
       `V.snoc`
       (nza' *. V.last vn1)
       V.++
       V.zipWith (\bl br -> bl*nza'*.br) v' vn2
         where
         nza' = fromNonZero a'
         (vn1,vn2) = V.splitAt (V.length vn - V.length v') vn
instance    (DivisionRing a, LeftModule a b)
         => MultiplicativeLeftAction (LeftTransformation a) (Vector b)


instance    DivisionRing a
         => MultiplicativeSemigroupLeftAction
             (LeftTransformation a)
             (LeftTransformationColumn a)
  where
  lt@(LeftTransformation nrs cs) *. (LeftTransformationColumn s a v) =
    LeftTransformationColumn s a' v'
    where
    nv = V.length v
    nvDiff = fromIntegral nrs - fromIntegral s - nv

    c1 = cs V.!? (nvDiff - 1)

    a' = maybe a ((*a) . ltHeadNonZero) c1
    alRecip = maybe one ltHeadRecip c1
    v' = V.zipWith (\bl br -> bl + br*alRecip) (maybe V.empty ltTail c1) $
         ltDrop nvDiff lt *. v

instance    DivisionRing a
         => MultiplicativeLeftAction
             (LeftTransformation a)
             (LeftTransformationColumn a)
