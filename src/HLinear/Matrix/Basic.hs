module HLinear.Matrix.Basic
where

import qualified Prelude as P
import HLinear.Utility.Prelude hiding ( one, zero, isOne, isZero, get, put )

import Data.Binary ( Binary, get, put )
import qualified Data.Permute as P
import qualified Data.Vector as V
import qualified Math.Structure as MS

import HLinear.Matrix.Column
import HLinear.Matrix.Definition
import HLinear.Utility.NmbRowColumn
import HLinear.Utility.Permute

--------------------------------------------------------------------------------
-- Eq, Show, NFData, Binary
--------------------------------------------------------------------------------

instance Eq a => Eq (Matrix a) where
  (Matrix nrs ncs rs) == (Matrix nrs' ncs' rs') =
    nrs == nrs' && ncs == ncs' && rs == rs'

instance Show a => Show (Matrix a) where
  show (Matrix 0 ncs rs) = "[ Matrix 0 x " <> show ncs <> " ]"
  show (Matrix nrs 0 rs) = "[ Matrix " <> show nrs <> " x 0 ]"
  show (Matrix _ _ rs) =
    V.foldl1 (\r r' -> r <> "\n" <> r') $ fmap show' shownEntries
      where
      shownEntries = fmap (fmap show) rs
      maxLength = V.maximum $ fmap (V.maximum . fmap length) shownEntries
      show' r= "[ " <> rShown <> " ]"
        where
        rShown = V.foldl1 (\a a' -> a <> " " <> a') $ fmap center r
      center s = P.replicate n ' ' <> s <> P.replicate n' ' '
        where
        n = (maxLength - length s) `P.div` 2
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
    V.forM_ rs $ mapM_ put

  get = do
    nrs <- get
    ncs <- get
    rs <- V.replicateM nrs $ V.replicateM ncs get
    return $ Matrix nrs ncs rs
  
--------------------------------------------------------------------------------
-- rows and columns
--------------------------------------------------------------------------------

instance HasNmbRows (Matrix a) where
  nmbRows (Matrix nrs _ _) = nrs

instance HasNmbCols (Matrix a) where
  nmbCols (Matrix _ ncs _) = ncs

(!) :: Matrix a -> Int -> Vector a
(!) = fromJust .: (!?)

(!?) :: Matrix a -> Int -> Maybe (Vector a)
(!?) (Matrix _ _ rs) = (V.!?) rs

--------------------------------------------------------------------------------
-- Permutation of rows and columns
--------------------------------------------------------------------------------

instance MultiplicativeSemigroupLeftAction P.Permute (Matrix a) where
  p *. (Matrix nrs ncs rs) =
    case compare np nrs of
      EQ -> Matrix nrs ncs $
              V.backpermute rs $ V.generate np $ \ix -> p `P.at` ix
      GT -> error "Permute *. Matrix: permutation size too big"
      -- fixme: let all permutations of size <= nrs act
      LT -> error "Permute *. Matrix: not implemented"
    where
      np = P.size p

instance MultiplicativeLeftAction P.Permute (Matrix a) where

instance MultiplicativeSemigroupRightAction P.Permute (Matrix a) where
  -- note: since we let permutations act from the left by default,
  -- the right action use the inverse of p
  (Matrix nrs ncs rs) .* p =
    case compare np ncs of
      EQ -> Matrix nrs ncs $ 
              fmap (`V.backpermute` pinvVector) rs
      GT -> error "Matrix .* Permute: permutation size too big"
      -- fixme: let all permutations of size <= ncs act
      LT -> error "Matrix .* Permute: not implemented"
    where
      np = P.size p
      pinv = recip p
      pinvVector = V.generate np $ \ix -> pinv `P.at` ix

instance MultiplicativeRightAction P.Permute (Matrix a)

transpose :: Matrix a -> Matrix a
transpose (Matrix nrs ncs rs) = Matrix ncs nrs $
  V.generate ncs $ \ix ->
    V.generate nrs $ \jx ->
      rs V.! jx V.! ix

--------------------------------------------------------------------------------
-- creation
--------------------------------------------------------------------------------

diagonal ::
     AdditiveMonoid a
  => Vector a -> Matrix a
diagonal ds =
  Matrix nrs nrs $
    (`V.imap` ds) $ \ix d ->
    V.generate nrs $ \jx ->
      if ix==jx then d else MS.zero
  where
    nrs = V.length ds

zero ::
     AdditiveMonoid a
  => Int -> Int -> Matrix a
zero nrs ncs
  | nrs >= 0 && ncs >= 0 =
      Matrix nrs ncs $ V.replicate nrs $ V.replicate ncs MS.zero
  | nrs < 0 = error "Matrix.zero: negative nrs"
  | ncs < 0 = error "Matrix.zero: negative ncs"

one ::
     ( AdditiveMonoid a, MultiplicativeMonoid a )
  => Int -> Matrix a
one nrs
  | nrs >= 0 = diagonal $ V.replicate nrs MS.one
  | nrs < 0 = error "Matrix.one: negative nrs"

elementary
  :: Ring a
  => Int -> Int -> Int -> Int -> Matrix a
elementary nrs ncs ix jx
  | nrs >= 0 && ncs >= 0 =
      Matrix nrs ncs $
        V.generate nrs $ \ix' ->
          if ix == ix'
          then V.generate ncs $ \jx' ->
                 if jx == jx' then MS.one else MS.zero
          else V.replicate ncs MS.zero
  | nrs < 0 = error "Matrix.elementary: negative nrs"
  | ncs < 0 = error "Matrix.elementary: negative ncs"

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

fromVectors = either error id . fromVectorsSafe
fromVectors' = either error id .:. fromVectorsSafe'

fromLists = either error id . fromListsSafe
fromLists' = either error id .:. fromListsSafe'

fromColumns = either error id . fromColumnsSafe
fromColumns' = either error id .:. fromColumnsSafe'


fromVectorsSafe :: Vector (Vector a) -> Either String (Matrix a)
fromVectorsSafe rs = 
  fromVectorsSafe' nrs ncs rs
    where
    nrs = V.length rs
    ncs = if nrs == 0 then 0 else V.length (V.head rs)

fromVectorsSafe'
  :: Int -> Int -> Vector (Vector a)
  -> Either String (Matrix a)
fromVectorsSafe' nrs ncs rs
  | nrs /= V.length rs = Left
      "HLinear.Matrix fromVectors': incorrect number of rows"
  | any ((/=ncs) . V.length) rs = Left
      "HLinear.Matrix fromVectors': rows must have the same length"
  | otherwise = Right $ Matrix nrs ncs rs


fromListsSafe :: [[a]] -> Either String (Matrix a)
fromListsSafe = fromVectorsSafe . fmap V.fromList . V.fromList

fromListsSafe' :: Int -> Int -> [[a]]
           -> Either String (Matrix a)
fromListsSafe' nrs ncs = fromVectorsSafe' nrs ncs . fmap V.fromList . V.fromList


fromColumnsSafe :: Vector (Column a) -> Either String (Matrix a)
fromColumnsSafe cs = fromColumnsSafe' nrs ncs cs
  where
    ncs = V.length cs
    nrs = if ncs == 0 then 0 else V.length (fromColumn $ V.head cs)

fromColumnsSafe'
  :: Int -> Int -> Vector (Column a)
  -> Either String (Matrix a)
fromColumnsSafe' nrs ncs cs
  | ncs /= V.length cs = Left
      "HLinear.Matrix fromColumns': incorrect number of columns"
  | any ((/=nrs) . V.length . fromColumn) cs = Left
      "HLinear.Matrix fromColumns': columns must have the same length"
  | otherwise =
      let rs = V.generate nrs $ \ix ->
                 V.generate ncs $ \jx ->
                   (fromColumn $ cs V.! jx) V.! ix
      in  Right $ Matrix nrs ncs rs

--------------------------------------------------------------------------------
-- conversion of matrices to vectors or lists
--------------------------------------------------------------------------------

toVectors :: Matrix a -> Vector (Vector a)
toVectors (Matrix _ _ rs) = rs

toLists :: Matrix a -> [[a]]
toLists = V.toList . fmap V.toList . toVectors

toColumns :: Matrix a -> Vector (Column a)
toColumns (Matrix nrs ncs rs) =
  V.generate ncs $ \jx ->
    Column $ V.generate nrs $ \ix ->
      rs V.! ix V.! jx

--------------------------------------------------------------------------------
-- container functionality
--------------------------------------------------------------------------------

instance Functor Matrix where
  fmap f (Matrix nrs ncs rs) = Matrix nrs ncs $ fmap (fmap f) rs

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


type instance Element (Matrix a) = Vector a

instance MonoFunctor (Matrix a) where
  omap f (Matrix nrs ncs rs) = Matrix nrs ncs' rs'
    where
      ncs' = maybe ncs V.length $ headMay rs'
      rs' = fmap f rs
