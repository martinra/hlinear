module HLinear.VVMatrix.Utils
where

import Control.Applicative ( (<$>), (<*>) )
import Data.Composition ( (.:), (.:.) )
import Data.Maybe
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.VVMatrix.Definition


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
