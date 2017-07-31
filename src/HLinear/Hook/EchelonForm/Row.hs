module HLinear.Hook.EchelonForm.Row
where

import HLinear.Utility.Prelude hiding ( zero, length )
import qualified Prelude as P


import qualified Data.Vector as V
import qualified Math.Structure as MS

import HLinear.Matrix.Basic ()
import HLinear.Matrix.Definition ( Matrix(..) )


-- | A vector of rows, each set off by a number of zeros
data EchelonFormRow a =
  EchelonFormRow
    { offset :: !Int
    , row :: !(Vector a)
    }

--------------------------------------------------------------------------------
-- Show, Eq, and NFData
--------------------------------------------------------------------------------

deriving instance Show a => Show (EchelonFormRow a)

instance ( Eq a, DecidableZero a ) => Eq (EchelonFormRow a) where
  (EchelonFormRow o r) == (EchelonFormRow o' r') =
    V.all isZero (left <> left') && right == right'
    where
      maxo = max o o'
      (left,right, left',right') =
        case compare o o' of
          EQ -> (V.empty,r, V.empty,r')
          GT -> let (lf,rt) = V.splitAt (o-o') r'
                in  (V.empty,r, lf,rt) 
          LT -> let (lf,rt) = V.splitAt (o'-o) r
                in  (lf,rt, V.empty,r') 

instance NFData a => NFData (EchelonFormRow a) where
  rnf (EchelonFormRow o s) =
    seq (rnf o) $
    seq (fmap rnf s) ()

--------------------------------------------------------------------------------
-- attributes
--------------------------------------------------------------------------------

length :: EchelonFormRow a -> Int
length (EchelonFormRow o r) = o + V.length r

setLength :: Int -> EchelonFormRow a -> EchelonFormRow a
setLength ncs (EchelonFormRow o r) = EchelonFormRow o' r
  where
    o' = ncs - V.length r
  
(!) :: AdditiveMonoid a => EchelonFormRow a -> Int -> a
(!) er@(EchelonFormRow o v) ix
  | ix < o          = MS.zero
  | ix >= length er = error "EchelonFormRow.(!): out of range"
  | otherwise       = v V.! (ix-o)

--------------------------------------------------------------------------------
-- properties
--------------------------------------------------------------------------------

pivotIx :: DecidableZero a => EchelonFormRow a -> Maybe Int
pivotIx = pivotIx' 0

pivotIx' :: DecidableZero a => Int -> EchelonFormRow a -> Maybe Int
pivotIx' ix ef = fst <$> pivotIxEntry' ix ef

pivotIxEntry' :: DecidableZero a => Int -> EchelonFormRow a -> Maybe (Int,a)
pivotIxEntry' ix (EchelonFormRow o v)
  | ix > o    = do
      jx <- V.findIndex (not . isZero) (V.drop (ix-o) v)
      return (ix+jx, v V.! jx)
  | otherwise = do
      jx <- V.findIndex (not . isZero) v
      return (o+jx, v V.! jx)

--------------------------------------------------------------------------------
-- container
--------------------------------------------------------------------------------

instance Functor EchelonFormRow where
  fmap f (EchelonFormRow o r) = EchelonFormRow o $ fmap f r

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
  EchelonFormRow o'' $ V.zipWith f rR rR' <> V.zipWith f rL rL'
  where
    nr = V.length r
    nr' = V.length r'
    maxnr = max nr nr'
    maxl = max (length e) (length e')
    o'' = maxl - maxnr

    rR  = V.replicate (maxnr - nr) MS.zero
    rR'  = V.replicate (maxnr - nr') MS.zero
    rL = V.drop (maxnr - nr) r
    rL' = V.drop (maxnr - nr') r'

--------------------------------------------------------------------------------
-- creation
--------------------------------------------------------------------------------

zero :: Int -> EchelonFormRow a
zero o = EchelonFormRow o V.empty

singleton :: Vector a -> EchelonFormRow a
singleton = EchelonFormRow 0

toVector :: AdditiveMonoid a => EchelonFormRow a -> Vector a
toVector (EchelonFormRow o r) = V.replicate o MS.zero <> r

--------------------------------------------------------------------------------
-- normalization
--------------------------------------------------------------------------------

normalize
  :: ( DivisionRing a, DecidableZero a )
  => EchelonFormRow a -> EchelonFormRow a
normalize (EchelonFormRow o r) = EchelonFormRow o $
  case V.findIndex (not . isZero) r of
    Nothing -> r
    Just ix ->
      let (r1,r23) = V.splitAt ix r
          r2 = NonZero $ V.head r23
          r3 = V.tail r23
      in  r1 <> one `V.cons` fmap (fromNonZero (recip r2) *) r3

--------------------------------------------------------------------------------
-- block sums
--------------------------------------------------------------------------------

sumRow :: EchelonFormRow a -> Vector a -> EchelonFormRow a
sumRow (EchelonFormRow o v) v' = EchelonFormRow o $ v <> v'

--------------------------------------------------------------------------------
-- subrows
--------------------------------------------------------------------------------

splitAt
  ::  Int -> EchelonFormRow a
  -> (EchelonFormRow a, EchelonFormRow a)
splitAt ix (EchelonFormRow o r) 
  | ix >= o   = ( EchelonFormRow o leftr, EchelonFormRow 0 rightr )
  | otherwise = ( EchelonFormRow ix0 leftr
                , EchelonFormRow (o-ix0) rightr )
  where
    ix0 = max 0 ix
    (leftr,rightr) = V.splitAt (ix-o) r

--------------------------------------------------------------------------------
-- additive structure
--------------------------------------------------------------------------------

instance AdditiveMagma a => AdditiveMagma (EchelonFormRow a) where
  (EchelonFormRow o r) + (EchelonFormRow o' r') =
    EchelonFormRow (maxnr - V.length mr) mr
    where
      lr = V.length r
      lr' = V.length r'
      minlr = min lr lr'
      maxnr = max (o+lr) (o'+lr')
      mr = case compare lr lr' of
             EQ -> V.zipWith (+) r r'
             GT -> let (left,right) = V.splitAt (lr-lr') r
                   in left <> V.zipWith (+) right r'
             LT -> let (left,right) = V.splitAt (lr'-lr) r'
                   in left <> V.zipWith (+) r right

instance AdditiveSemigroup a => AdditiveSemigroup (EchelonFormRow a)
