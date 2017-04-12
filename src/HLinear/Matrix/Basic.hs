{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module HLinear.Matrix.Basic
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.DeepSeq ( NFData, rnf )
import Data.Binary
import Data.Composition ( (.:), (.:.) )
import Data.Maybe
import qualified Data.Permute as P
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Math.Structure as MS
import Math.Structure hiding ( one, zero, isOne, isZero )
import Numeric.Natural

import HLinear.Utility.Permute
import HLinear.Matrix.Definition


instance Eq a => Eq (Matrix a) where
  (Matrix nrs ncs rs) == (Matrix nrs' ncs' rs') =
    nrs == nrs' && ncs == ncs' && rs == rs'

instance Show a => Show (Matrix a) where
  show (Matrix 0 ncs rs) = "[ Matrix 0 x " ++ show ncs ++ " ]"
  show (Matrix nrs 0 rs) = "[ Matrix " ++ show nrs ++ " x 0 ]"
  show (Matrix _ _ rs) =
    V.foldl1 (\r r' -> r ++ "\n" ++ r') $ V.map show' shownEntries
      where
      shownEntries = V.map (V.map show) rs
      maxLength = V.maximum $ V.map (V.maximum . V.map length) shownEntries
      show' r= "[ " ++ rShown ++ " ]"
        where
        rShown = V.foldl1 (\a a' -> a ++ " " ++ a') $ V.map center r
      center s = replicate n ' ' ++ s ++ replicate n' ' '
        where
        n = (maxLength - length s) `div` 2
        n' = maxLength - n - length s

instance NFData a => NFData (Matrix a) where
  rnf (Matrix nrs ncs rs) =
    seq (rnf nrs) $
    seq (rnf ncs) $
    seq (rnf rs) ()

instance Binary a => Binary (Matrix a) where
  put (Matrix nrs ncs rs) = do
    put nrs
    put ncs
    V.forM_ rs $ V.mapM_ put

  get = do
    nrs <- get
    ncs <- get
    rs <- V.replicateM (fromIntegral nrs) $ V.replicateM (fromIntegral ncs) get
    return $ Matrix nrs ncs rs
  
--------------------------------------------------------------------------------
-- row access
--------------------------------------------------------------------------------

(!) :: Matrix a -> Int -> Vector a
(!) = fromJust .: (!?)

(!?) :: Matrix a -> Int -> Maybe (Vector a)
(!?) = (V.!?) . rows

--------------------------------------------------------------------------------
-- Permutation of rows and columns
--------------------------------------------------------------------------------

instance MultiplicativeSemigroupLeftAction P.Permute (Matrix a) where
  p *. (Matrix nrs ncs rs) =
    case compare np nrsZ of
      EQ -> Matrix nrs ncs $
              V.backpermute rs $ V.generate np $ \ix -> p `P.at` ix
      GT -> error "Permute *. Matrix: permutation size too big"
      -- fixme: let all permutations of size <= nrs act
      LT -> error "Permute *. Matrix: not implemented"
    where
      np = P.size p
      nrsZ = fromIntegral nrs

instance MultiplicativeLeftAction P.Permute (Matrix a) where

instance MultiplicativeSemigroupRightAction P.Permute (Matrix a) where
  (Matrix nrs ncs rs) .* p =
    case compare np ncsZ of
      EQ -> Matrix nrs ncs $ 
              V.map (\r -> V.backpermute r $ V.generate np $ \ix -> p `P.at` ix) rs
      GT -> error "Matrix .* Permute: permutation size too big"
      -- fixme: let all permutations of size <= ncs act
      LT -> error "Matrix .* Permute: not implemented"
    where
      np = P.size p
      ncsZ = fromIntegral ncs

instance MultiplicativeRightAction P.Permute (Matrix a) where

--------------------------------------------------------------------------------
-- creation
--------------------------------------------------------------------------------

diagonal ::
     AdditiveMonoid a
  => Vector a -> Matrix a
diagonal ds =
  Matrix nrs nrs $
    (`V.imap` ds) $ \ix d ->
    V.generate nrsZ $ \jx ->
      if ix==jx then d else MS.zero
  where
    nrsZ = V.length ds
    nrs = fromIntegral nrsZ

zero ::
     AdditiveMonoid a
  => Natural -> Natural -> Matrix a
zero nrs ncs =
  Matrix nrs ncs $
    V.replicate (fromIntegral nrs) $
    V.replicate (fromIntegral ncs) MS.zero

one ::
     ( AdditiveMonoid a, MultiplicativeMonoid a )
  => Natural -> Matrix a
one = diagonal . (`V.replicate` MS.one) . fromIntegral

--------------------------------------------------------------------------------
-- predicates
--------------------------------------------------------------------------------

isZero ::
     DecidableZero a
  => Matrix a -> Bool
isZero (Matrix _ _ vs) = V.all (V.all MS.isZero) vs

isOne ::
     ( DecidableZero a, DecidableOne a )
  => Matrix a -> Bool
isOne (Matrix _ _ vs) =
  V.and $ V.imap (\ix ->
    V.and . V.imap (\jx ->
      if ix == jx then MS.isOne else MS.isZero
  )) vs

--------------------------------------------------------------------------------
-- construction of matrices from vectors or lists
--------------------------------------------------------------------------------

fromVectors :: Vector (Vector a) -> Either String (Matrix a)
fromVectors rs = 
  fromVectors' nrs ncs rs
    where
    nrs = fromIntegral $ V.length rs
    ncs = if nrs == 0 then 0 else fromIntegral $ V.length (V.head rs)

fromVectors' :: Natural -> Natural -> Vector (Vector a)
             -> Either String (Matrix a)
fromVectors' nrs ncs rs
  | nrs /= fromIntegral (V.length rs) = Left
      "HLinear.Matrix fromVectors': incorrect number of rows"
  | any ((/=ncs) . fromIntegral . V.length) rs = Left
      "HLinear.Matrix fromVectors': rows must have the same length"
  | otherwise = Right $ Matrix nrs ncs rs

fromLists :: [[a]] -> Either String (Matrix a)
fromLists = fromVectors . V.map V.fromList . V.fromList

fromLists' :: Natural -> Natural -> [[a]]
           -> Either String (Matrix a)
fromLists' nrs ncs = fromVectors' nrs ncs . V.map V.fromList . V.fromList

fromVectorsUnsafe = either undefined id . fromVectors
fromVectorsUnsafe' = either undefined id .:. fromVectors'

fromListsUnsafe = either undefined id . fromLists
fromListsUnsafe' = either undefined id .:. fromLists'

--------------------------------------------------------------------------------
-- container functionality
--------------------------------------------------------------------------------

instance Functor Matrix where
  fmap f (Matrix nrs ncs rs) = Matrix nrs ncs $ V.map (V.map f) rs

instance Foldable Matrix where
  foldl f a (Matrix nrs ncs rs) = foldl (\a' r -> foldl f a' r) a rs
  foldr f a (Matrix nrs ncs rs) = foldr (\r a' -> foldr f a' r) a rs

instance Traversable Matrix where
  traverse f (Matrix nrs ncs rs) = Matrix nrs ncs <$> traverse (traverse f) rs
  sequenceA (Matrix nrs ncs rs) = Matrix nrs ncs <$> sequenceA (fmap sequenceA rs)

zipWith :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
zipWith f (Matrix nrs ncs rs) (Matrix nrs' ncs' rs')
  | nrs /= nrs' || ncs /= ncs' = error "Matrix.zipWith: incompatible dimensions"
  | otherwise = Matrix nrs ncs $ V.zipWith (V.zipWith f) rs rs'
