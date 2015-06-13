module HLinear.Matrix.Conversion
where

import Control.Monad.Reader
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural

import HLinear.BRMatrix.Definition ( BRMatrix(..) )
import HLinear.BRMatrix.RVector ( RVector(..) )
import qualified HLinear.BRMatrix.RVector as RV
import qualified HLinear.Matrix.Algebra as MA
import HLinear.Matrix.Definition ( Matrix(..) )


toBRMatrix :: Matrix a -> BRMatrix a
toBRMatrix (Matrix nrs ncs rs) = BRMatrix nrs ncs $ RVector $ V.map RVector rs

fromBRMatrix :: DecidableZero a => BRMatrix a -> Matrix a
fromBRMatrix (BRMatrix nrs ncs rs) = Matrix nrs ncs $
  V.map (RV.fromRVectorUnsafe ncs) $ RV.fromRVectorUnsafe nrs rs

fromBRMatrixUnsafe :: ( DecidableZero a, AdditiveMonoid a )
                   => Natural -> Natural -> BRMatrix a -> Matrix a
fromBRMatrixUnsafe nrs ncs (BRMatrix nrs' ncs' rs) = Matrix nrs ncs $
  flip runReader ncs $ MA.unMRow $ V.sequence $
  RV.fromRVectorUnsafe nrs $
  RV.liftRV (V.map $ return . RV.fromRVectorUnsafe ncs) rs
