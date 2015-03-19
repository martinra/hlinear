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
import Numeric.Natural ( Natural )

import HLinear.VVMatrix.Definition ( VVMatrix(..) )


nmbRows :: VVMatrix a -> Maybe Natural
nmbRows (Zero nrs _) = nrs
nmbRows (One nrs _)  = nrs
nmbRows (VVMatrix nrs _ _) = Just nrs

nmbCols :: VVMatrix a -> Maybe Natural
nmbCols (Zero _ ncs) = ncs
nmbCols (One ncs _)  = ncs
nmbCols (VVMatrix _ ncs _) = Just ncs


cmbDim :: Natural -> Natural -> Maybe Natural
cmbDim d d' | d == d' = Just d
            | otherwise = Nothing

cmbDim' :: Natural -> Natural -> Natural
cmbDim' = fromJust .: cmbDim

cmbDimMay :: Natural -> Maybe Natural -> Maybe Natural
cmbDimMay d (Just d') | d == d'   = Just d
                      | otherwise = Nothing
cmbDimMay d Nothing   = Just d

cmbDimMay' :: Natural -> Maybe Natural -> Natural
cmbDimMay' = fromJust .: cmbDimMay

cmbDimMMayGeneral :: Maybe Natural
                  -> Maybe Natural -> Maybe Natural -> Maybe Natural
cmbDimMMayGeneral def (Just d) (Just d') | d == d'   = Just d
                                         | otherwise = def 
cmbDimMMayGeneral _ (Just d) Nothing  = Just d
cmbDimMMayGeneral _ Nothing (Just d') = Just d'
cmbDimMMayGeneral _ Nothing Nothing = Nothing

cmbDimMMay :: Maybe Natural -> Maybe Natural -> Maybe Natural
cmbDimMMay = cmbDimMMayGeneral Nothing

cmbDimMMay' :: Maybe Natural -> Maybe Natural -> Maybe Natural
cmbDimMMay' = cmbDimMMayGeneral $ error "incompatible dimensions"


zeroMatrix :: AdditiveMonoid a
           => Natural -> Natural -> VVMatrix a
zeroMatrix nrs ncs = VVMatrix nrs ncs $
                       V.replicate (fromIntegral nrs) $
                       V.replicate (fromIntegral ncs) zero

diagonalMatrix :: AdditiveMonoid a
               => Vector a -> VVMatrix a
diagonalMatrix ds = VVMatrix (fromIntegral nrs) (fromIntegral nrs) $
                      (`V.imap` ds) $ \ix d ->
                      V.generate nrs $ \jx ->
                        if ix==jx then d else zero
  where
  nrs = V.length ds

identityMatrix :: ( AdditiveMonoid a, MultiplicativeMonoid a )
               => Natural -> VVMatrix a
identityMatrix = diagonalMatrix . (`V.replicate` one) . fromIntegral


forceVVMay :: AdditiveMonoid a
           => VVMatrix a -> Maybe (VVMatrix a)
forceVVMay m@(VVMatrix _ _ _)           = Just m
forceVVMay (Zero (Just nrs) (Just ncs)) = Just $ zeroMatrix nrs ncs
forceVVMay (One (Just nrs) a )          = Just $ diagonalMatrix $
                                               V.replicate (fromIntegral nrs) a
forceVVMay _ = Nothing


forceSize :: AdditiveMonoid a
          => Natural -> Natural -> VVMatrix a -> VVMatrix a
forceSize = fromJust .:. forceSizeMay

forceSizeMay :: AdditiveMonoid a
             => Natural -> Natural -> VVMatrix a -> Maybe (VVMatrix a)
forceSizeMay nrs ncs (Zero nrs' ncs') =
  zeroMatrix <$> cmbDimMay nrs nrs' <*> cmbDimMay ncs ncs'
forceSizeMay nrs ncs (One nrs' a) =
  cmbDimMay ncs nrs' >>
  diagonalMatrix . (`V.replicate` a) <$>
    fromIntegral <$> cmbDimMay nrs nrs'
forceSizeMay nrs ncs m@(VVMatrix nrs' ncs' _) =
  cmbDim nrs nrs' >> cmbDim ncs ncs' >>
  return m


toVectors :: AdditiveMonoid a
          => VVMatrix a -> Maybe ( Vector (Vector a) )
toVectors (VVMatrix _ _ rs) = Just rs
toVectors m = forceVVMay >=> toVectors $ m

fromVectors :: Vector (Vector a) -> VVMatrix a
fromVectors rs =
  -- todo: introduce unsafe version of fromVectors'
  fromVectors' nrs ncs rs
  where
  nrs = fromIntegral $ V.length rs
  ncs = if nrs == 0 then 0 else fromIntegral $ V.length (V.head rs)

fromVectors' :: Natural -> Natural -> Vector (Vector a) -> VVMatrix a
fromVectors' nrs ncs rs
  | nrs /= fromIntegral (V.length rs) =
      error $ "HLinear.VVMatrix fromVectors': " ++
              "number of rows incorrect"
  | any ((/=ncs) . fromIntegral . V.length) rs =
      error $ "HLinear.VVMatrix fromVectors': " ++
              "rows must have the same length"
  | otherwise = VVMatrix nrs ncs rs


toLists :: AdditiveMonoid a
        => VVMatrix a -> Maybe [[a]]
toLists = toVectors >=> return . V.toList . V.map V.toList

fromLists :: [[a]] -> VVMatrix a
fromLists = fromVectors . V.map V.fromList . V.fromList

fromLists' :: Natural -> Natural -> [[a]] -> VVMatrix a
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
