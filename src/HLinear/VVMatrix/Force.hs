module HLinear.VVMatrix.Force
where

import Control.Monad ( (>=>) )
import Data.Composition ( (.:.) )
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector ( Vector )
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.VVMatrix.Creation
import HLinear.VVMatrix.Definition ( VVMatrix(..) )
import HLinear.VVMatrix.Utils


toVectors :: AdditiveMonoid a
          => VVMatrix a -> Maybe ( Vector (Vector a) )
toVectors (VVMatrix _ _ rs) = Just rs
toVectors m = forceVVMay >=> toVectors $ m

toLists :: AdditiveMonoid a
        => VVMatrix a -> Maybe [[a]]
toLists = toVectors >=> return . V.toList . V.map V.toList


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
