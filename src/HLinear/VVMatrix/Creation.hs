module HLinear.VVMatrix.Creation
where

import Control.Applicative ( (<$>) )
import Control.Monad ( (>=>) )
import Data.Composition ( (.:.) )
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector ( Vector )
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.VVMatrix.Definition ( VVMatrix(..) )
import HLinear.VVMatrix.Utils


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


zeroMatrix' :: Maybe Natural -> Maybe Natural -> VVMatrix a
zeroMatrix' = Zero

oneMatrix' :: Maybe Natural -> a -> VVMatrix a
oneMatrix' = One


forceVV :: AdditiveMonoid a
        => VVMatrix a -> VVMatrix a
forceVV = fromJust . forceVVMay

forceVVMay :: AdditiveMonoid a
           => VVMatrix a -> Maybe (VVMatrix a)
forceVVMay m@VVMatrix{}                 = Just m
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
