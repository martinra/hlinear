{-# LANGUAGE
    StandaloneDeriving
  #-}

module HLinear.PLE.Hook.EchelonForm.Row
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      , length
                      )

import Control.DeepSeq
import Control.Arrow ( first )
import qualified Data.Vector as V
import Data.Vector ( Vector(..) )
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.Matrix.Basic ()
import HLinear.Matrix.Definition ( Matrix(..) )


-- | A vector of rows, each set off by a number of zeros
data EchelonFormRow a =
  EchelonFormRow
    { offset :: Natural
    , row :: Vector a
    }

-- properties

pivotIx :: DecidableZero a => EchelonFormRow a -> Maybe Int
pivotIx (EchelonFormRow o v) = (oZ+) <$> V.findIndex (not . isZero) v
  where
    oZ = fromIntegral o


pivotIx' :: DecidableZero a => EchelonFormRow a -> Int -> Maybe Int
pivotIx' (EchelonFormRow o v) ix = (oZ+) <$> V.findIndex (not . isZero) (V.drop (ix - oZ) v)
  where
    oZ = fromIntegral o

-- Show, Eq, and NFData

deriving instance Show a => Show (EchelonFormRow a)

instance ( Eq a, DecidableZero a ) => Eq (EchelonFormRow a) where
  (EchelonFormRow o r) == (EchelonFormRow o' r') =
    V.all isZero (left V.++ left') && right == right'
    where
      maxo = max o o'
      (left,right, left',right') =
        case compare o o' of
          EQ -> (V.empty,r, V.empty,r')
          GT -> let (lf,rt) = V.splitAt (fromIntegral o - fromIntegral o') r'
                in (V.empty,r, lf,rt) 
          LT -> let (lf,rt) = V.splitAt (fromIntegral o' - fromIntegral o) r
                in (lf,rt, V.empty,r') 

instance NFData a => NFData (EchelonFormRow a) where
  rnf (EchelonFormRow o s) =
    seq (rnf o) $
    seq (V.map rnf s) $ ()

-- length

length :: EchelonFormRow a -> Int
length (EchelonFormRow o r) = fromIntegral o + V.length r

setLength :: Int -> EchelonFormRow a -> EchelonFormRow a
setLength ncs (EchelonFormRow o r)
  | o' < 0 = error "EchelonFormRow.setLength: to long row"
  | otherwise = EchelonFormRow (fromIntegral o') r
  where
    o' = ncs - V.length r
  
-- access and conversion

(!) :: AdditiveMonoid a => EchelonFormRow a -> Int -> a
(!) er@(EchelonFormRow o v) ix
  | ix < oZ         = zero
  | ix >= length er = error "EchelonFormRow.(!): out of range"
  | otherwise       = v V.! (ix-oZ)
  where
    oZ = fromIntegral o

-- container

instance Functor EchelonFormRow where
  fmap f (EchelonFormRow o r) = EchelonFormRow o $ V.map f r

instance Foldable EchelonFormRow where
  foldl f a (EchelonFormRow o r) = V.foldl f a r
  foldr f a (EchelonFormRow o r) = V.foldr f a r

instance Traversable EchelonFormRow where
  traverse f (EchelonFormRow o s) = EchelonFormRow o <$> traverse f s

zipWith
  :: ( AdditiveMonoid a, AdditiveMonoid b )
  => (a -> b -> c) -> EchelonFormRow a -> EchelonFormRow b
  -> EchelonFormRow c
zipWith f e@(EchelonFormRow o r) e'@(EchelonFormRow o' r') =
  EchelonFormRow o'' $ V.zipWith f rR rR' V.++ V.zipWith f rL rL'
  where
    nr = V.length r
    nr' = V.length r'
    maxnr = max nr nr'
    maxl = max (length e) (length e')
    o'' = fromIntegral $ maxl - maxnr

    rR  = V.replicate (maxnr - nr) zero
    rR'  = V.replicate (maxnr - nr') zero
    rL = V.drop (maxnr - nr) r
    rL' = V.drop (maxnr - nr') r'

-- creation

zeroEFR :: Natural -> EchelonFormRow a
zeroEFR o = EchelonFormRow o V.empty

toVector :: AdditiveMonoid a => EchelonFormRow a -> Vector a
toVector (EchelonFormRow o r) = V.replicate (fromIntegral o) zero V.++ r

-- subrows

splitAt
  ::  Int -> EchelonFormRow a
  -> (EchelonFormRow a, EchelonFormRow a)
splitAt ix (EchelonFormRow o r) 
  | ix >= oZ  = ( EchelonFormRow o leftr, EchelonFormRow 0 rightr )
  | otherwise = ( EchelonFormRow ix0N leftr
                , EchelonFormRow (fromIntegral $oZ-ix0) rightr )
  where
    ix0 = max 0 ix
    ix0N = fromIntegral ix0
    oZ = fromIntegral o
    (leftr,rightr) = V.splitAt (ix-oZ) r

-- additive structure

instance AdditiveMagma a => AdditiveMagma (EchelonFormRow a) where
  (EchelonFormRow o r) + (EchelonFormRow o' r') =
    EchelonFormRow (fromIntegral $ maxnr - V.length mr) mr
    where
      lr = V.length r
      lr' = V.length r'
      minlr = min lr lr'
      maxnr = max (fromIntegral o + lr) (fromIntegral o' + lr')
      mr = case compare lr lr' of
             EQ -> V.zipWith (+) r r'
             GT -> let (left,right) = V.splitAt (lr-lr') r
                   in left V.++ V.zipWith (+) right r'
             LT -> let (left,right) = V.splitAt (lr'-lr) r'
                   in left V.++ V.zipWith (+) r right

instance AdditiveSemigroup a => AdditiveSemigroup (EchelonFormRow a)

-- block sums

sumRow :: EchelonFormRow a -> Vector a -> EchelonFormRow a
sumRow (EchelonFormRow o v) v' = EchelonFormRow o $ v V.++ v'
