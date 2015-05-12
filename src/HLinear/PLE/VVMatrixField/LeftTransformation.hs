{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  #-}

module HLinear.PLE.VVMatrixField.LeftTransformation
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Applicative ( (<$>) )
import Data.Maybe
import Data.Vector ( Vector(..) )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ( Natural )

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

ltHead' :: LeftTransformationColumn a -> NonZero a
ltHead' (LeftTransformationColumn _ a _) = a

ltHeadRecip :: forall a . MultiplicativeGroup (NonZero a)
            => LeftTransformationColumn a -> a  
ltHeadRecip (LeftTransformationColumn _ a _) = fromNonZero $ recip (a :: NonZero a)


ltTail :: LeftTransformationColumn a -> Vector a
ltTail (LeftTransformationColumn _ _ v) = v

applyToColumn :: ( DivisionRing a, LeftModule a b )
              => LeftTransformation a -> Vector b
              -> Vector b
-- todo: I'm not sure this is the most efficient way. Compare to fold over vectors.
applyToColumn lt@(LeftTransformation nrs cs) v =
  V.generate (fromIntegral nrs) $ \ix ->
    V.foldl' (+) (av V.! ix) $
      V.zipWith (\c b -> (c!ix) *. b) (V.take (ix-1) cs) av
  where
  av = V.zipWith (*.) (V.map ltHead cs) v
       `mappend`
       (V.length cs `V.drop` v)


 -- | in the PLE decomopition a left transformation corresponds to the inverse
 --   of the matrix given above
 --    a1^-1         0           0      0
 --   -a2^-1*v   a2^-1           0      0
 --   -a3^-1*v  -a3^-1*v   a3^-1        0
 --   -a4^-1*v  -a4^-1*v  -a4^-1*v  a4^-1

-- toVVMatrix :: DivisionRing a
--            => LeftTransformation a -> VVMatrix a
-- toVVMatrix (LeftTransformation nrs cs) = 
--   VVMatrix nrs nrs $
--     V.generate (fromIntegral nrs) $ \ix ->
--       let arecip = maybe one ltHeadRecip $ cs V.!? ix
--       in V.generate (fromIntegral nrs) $ \jx ->
--         case compare ix jx of
--           LT -> zero
--           EQ -> if jx < ncs
--                 then arecip
--                 else one
--           GT -> if jx < ncs
--                 then (arecip*) $ negate (cs V.! jx ! ix)
--                 else zero
--   where
--   ncs = V.length cs


nmbRows :: LeftTransformation a -> Natural
nmbRows (LeftTransformation nrs _) = nrs

nmbCols :: LeftTransformation a -> Natural
nmbCols (LeftTransformation nrs _) = nrs


-- | Multiplying a smaller LT from the RIGHT with a bigger one.
--   This means simply concatenating columns.
concat :: LeftTransformation a -> LeftTransformation a
       -> LeftTransformation a
concat (LeftTransformation nrs cs) (LeftTransformation nrs' cs') =
  fromJust $ cmbDim nrs (nrs' + fromIntegral (V.length cs)) >>
  return ( LeftTransformation nrs $
           cs V.++ V.map (ltShiftOffset $ V.length cs) cs'
         )

-- | Multiplying a smaller LT from the LEFT with a bigger one.
--   This means the left transformation has to be applied to the columns of the
--   transformation that is to the right.
merge :: DivisionRing a
      => LeftTransformation a -> LeftTransformation a
      -> LeftTransformation a
merge lt@(LeftTransformation nrs cs) (LeftTransformation nrs' cs') =
  fromJust $ cmbDim nrs' (nrs + fromIntegral (V.length cs')) >>
  return ( LeftTransformation nrs' $
           V.map (applyToLTColumn lt) cs'
           V.++
           V.map (ltShiftOffset $ V.length cs') cs
         )

applyToLTColumn :: DivisionRing a
                => LeftTransformation a -> LeftTransformationColumn a
                -> LeftTransformationColumn a
applyToLTColumn (LeftTransformation nrs cs) c@(LeftTransformationColumn offset a vs)
  | offset > V.length cs = c
  | otherwise = LeftTransformationColumn offset a' vs''
  where
  ncs' = fromIntegral $ (fromIntegral nrs :: Int) P.- offset P.- 1
  a' = ltHead' (cs V.! offset) * a
  -- todo: this should be truncateLeftTransformation
  vs' = applyToColumn (LeftTransformation  ncs' ((offset+1) `V.drop` cs)) vs
  vs'' =  V.zipWith (\b b' -> b+b'*(fromNonZero a')) vs' $ ltTail $ cs V.! offset 

-- apply :: Field a
--       => LeftTransformation a -> VVMatrix a
--       -> VVMatrix a
-- apply (LeftTransformation nrs cs) (VVMatrix nrs' ncs' rs) =
--   fromJust $ cmbDim nrs' nrs >>
--   return ( VVMatrix nrs' ncs' rsL )
--   where
--   ncs = V.length cs
-- 
--   rsScaled = V.zipWith ( \(a,_) r -> V.map (fromNonZero a *) r )
--              cs rs
--   rsL = V.generate (fromIntegral nrs) $ \ix ->
--           V.foldl' (V.zipWith (+))
--             ( if ix < ncs then rsScaled ! ix else rs ! ix )
--             ( V.zipWith (\(_,c) r -> V.map ((c ! (ix-1)) *) r)
--                 (V.take (ix-1) cs) rsScaled )
