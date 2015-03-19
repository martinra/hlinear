module HLinear.VVMatrix.Basic
where

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( (>=>) )
import Data.Composition ( (.:), (.:.) )
import Data.Maybe ( fromMaybe, fromJust, maybe )
import qualified Data.Vector as V
import Data.Vector ( Vector )
import Math.Structure ( DecidableZero, isZero
                      , DecidableOne, isOne
                      , AdditiveMonoid, zero
                      , MultiplicativeMonoid, one
                      )

import HLinear.VVMatrix.Definition ( VVMatrix(..) )


nmbRows :: VVMatrix a -> Maybe Int
nmbRows (Zero nrs _) = nrs
nmbRows (One nrs _)  = nrs
nmbRows (VVMatrix nrs _ _) = Just nrs

nmbCols :: VVMatrix a -> Maybe Int
nmbCols (Zero _ ncs) = ncs
nmbCols (One ncs _)  = ncs
nmbCols (VVMatrix _ ncs _) = Just ncs


cmbDim :: Int -> Int -> Maybe Int
cmbDim d d' | d == d' = Just d
            | otherwise = Nothing

cmbDim' :: Int -> Int -> Int
cmbDim' = fromJust .: cmbDim

cmbDimMay :: Int -> Maybe Int -> Maybe Int
cmbDimMay d (Just d') | d == d'   = Just d
                      | otherwise = Nothing
cmbDimMay d Nothing   = Just d

cmbDimMay' :: Int -> Maybe Int -> Int
cmbDimMay' = fromJust .: cmbDimMay

cmbDimMMayGeneral :: Maybe Int
                  -> Maybe Int -> Maybe Int -> Maybe Int
cmbDimMMayGeneral def (Just d) (Just d') | d == d'   = Just d
                                         | otherwise = def 
cmbDimMMayGeneral _ (Just d) Nothing  = Just d
cmbDimMMayGeneral _ Nothing (Just d') = Just d'
cmbDimMMayGeneral _ Nothing Nothing = Nothing

cmbDimMMay :: Maybe Int -> Maybe Int -> Maybe Int
cmbDimMMay = cmbDimMMayGeneral Nothing

cmbDimMMay' :: Maybe Int -> Maybe Int -> Maybe Int
cmbDimMMay' = cmbDimMMayGeneral $ error "incompatible dimensions"


-- fixme: we already assume Natural instead of Int as a type
zeroMatrix :: AdditiveMonoid a
           => Int -> Int -> VVMatrix a
zeroMatrix nrs ncs = VVMatrix nrs ncs $ V.replicate nrs $
                                        V.replicate ncs zero

diagonalMatrix :: AdditiveMonoid a
               => Vector a -> VVMatrix a
diagonalMatrix ds = VVMatrix nrs nrs $ (`V.imap` ds) $ \ix d ->
                                       V.generate nrs $ \jx ->
                                         if ix==jx then d else zero
  where
  nrs = V.length ds

identityMatrix :: ( AdditiveMonoid a, MultiplicativeMonoid a )
               => Int -> VVMatrix a
identityMatrix = diagonalMatrix . (`V.replicate` one)


forceVVMay :: AdditiveMonoid a
           => VVMatrix a -> Maybe (VVMatrix a)
forceVVMay m@(VVMatrix _ _ _)           = Just m
forceVVMay (Zero (Just nrs) (Just ncs)) = Just $ zeroMatrix nrs ncs
forceVVMay (One (Just nrs) a )          = Just $ diagonalMatrix $
                                               V.replicate nrs a
forceVVMay _ = Nothing


forceSize :: AdditiveMonoid a
          => Int -> Int -> VVMatrix a -> VVMatrix a
forceSize = fromJust .:. forceSizeMay

forceSizeMay :: AdditiveMonoid a
             => Int -> Int -> VVMatrix a -> Maybe (VVMatrix a)
forceSizeMay nrs ncs (Zero nrs' ncs') =
  zeroMatrix <$> cmbDimMay nrs nrs' <*> cmbDimMay ncs ncs'
forceSizeMay nrs ncs (One nrs' a) =
  cmbDimMay ncs nrs' >>
  diagonalMatrix . (`V.replicate` a) <$> cmbDimMay nrs nrs'
forceSizeMay nrs ncs m@(VVMatrix nrs' ncs' _) =
  cmbDim nrs nrs' >> cmbDim ncs ncs' >>
  return m


toVectors :: VVMatrix a -> Maybe ( Vector (Vector a) )
toVector (Zero nrs ncs) = do
  nrs' <- nrs
  ncs' <- ncs
  return $ V.replicate nrs' $ V.replicate ncs' zero
toVector (One nrs a) = do
  nrs' <- nrs
  return $ V.replicate nrs' $ V.replicate nrs' a
toVectors (VVMatrix _ _ rs) = Just rs

fromVectors :: Vector (Vector a) -> VVMatrix a
fromVectors rs =
  -- todo: introduce unsafe version of fromVectors'
  fromVectors' nrs (if nrs == 0 then 0 else V.length (V.head rs)) rs
  where
  nrs = V.length rs

fromVectors' :: Int -> Int -> Vector (Vector a) -> VVMatrix a
fromVectors' nrs ncs rs
  | nrs /= V.length rs = error $ "HLinear.VVMatrix fromVectors': " ++
                                "number of rows incorrect"
  | any ((/=ncs) . V.length) rs = error $ "HLinear.VVMatrix fromVectors': " ++
                                          "rows must have the same length"
  | otherwise =  VVMatrix nrs ncs rs


toLists :: VVMatrix a -> Maybe [[a]]
toLists = toVectors >=> return . V.toList . V.map V.toList

fromLists :: [[a]] -> VVMatrix a
fromLists = fromVectors . V.map V.fromList . V.fromList

fromLists' :: Int -> Int -> [[a]] -> VVMatrix a
fromLists' nrs ncs = fromVectors' nrs ncs . V.map V.fromList . V.fromList


instance   ( DecidableZero a, DecidableOne a, Eq a)
         =>  Eq (VVMatrix a) where
  (VVMatrix nrs ncs rs) == (VVMatrix nrs' ncs' rs') =
    nrs == nrs' && ncs == ncs' && rs == rs'

  (Zero nrs ncs) == (Zero nrs' ncs') =
    nrs == nrs' && ncs == ncs'
  (Zero nrs ncs) == (One nrs' a') =
    nrs == nrs' && ncs == nrs' && isZero a'
  (One nrs a) == (Zero nrs' ncs') =
    nrs == nrs' && nrs == ncs' && isZero a
  (Zero nrs ncs) == (VVMatrix nrs' ncs' rs) =
      maybe True (==nrs') nrs
    && maybe True (==ncs') ncs
    && V.all (V.all isZero) rs
  m == m'@(Zero _ _) = m' == m

  (One nrs a) == (One nrs' a') = 
       ( fromMaybe True $ (==) <$> nrs <*> nrs' )
    && a == a'
  (One nrs a) == (VVMatrix nrs' ncs' rs') =
       maybe True (==nrs') nrs
    && maybe True (==ncs') nrs
    && (`iall` rs') ( \ix -> iall
                    ( \jx a -> if jx/=ix then isZero a else isOne a ) )
    where
    iall :: (Int -> a -> Bool) -> Vector a -> Bool
    iall f = V.ifoldr' (\ix a b -> b && f ix a) True
  m == m'@(One _ _) = m' == m


instance ( AdditiveMonoid a, Show a ) => Show (VVMatrix a) where
  show m@(Zero nrs ncs) =
    ( \str -> maybe str show $ forceVVMay m )
      ("VVMatrix.Zero " ++ show nrs ++ " " ++ show ncs )
  show m@(One nrs a)    =
    ( \str -> maybe str  show $ forceVVMay m )
      ("VVMatrix.One " ++ show nrs ++ " " ++ show a)
  show (VVMatrix 0 _ rs) = "[ ]"
  show (VVMatrix _ 0 rs) = "[ ]"
  show (VVMatrix _ _ rs) =
    V.foldl1 (\r r' -> r ++ "\n" ++ r') $ V.map show' rsShown
    where
    rsShown = V.map (V.map show) rs
    show' r= "[ " ++ rShown ++ " ]"
      where
      rShown = V.foldl1 (\a a' -> a ++ " " ++ a') $ V.map center r
    center s = replicate n ' ' ++ s ++ replicate n' ' '
      where
      maxLength = V.maximum $ V.map (V.maximum . V.map length) rsShown
      n = (maxLength - length s) `div` 2
      n' = maxLength - n - length s
