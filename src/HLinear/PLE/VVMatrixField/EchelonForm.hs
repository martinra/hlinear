module HLinear.PLE.VVMatrixField.EchelonForm
where

-- import Prelude hiding ( (-), negate, subtract
--                       , (*), (/), recip, (^), (^^)
--                       , gcd
--                       , quotRem, quot, rem
--                       )
import Control.Arrow ( first )
import qualified Data.Vector as V
import Data.Vector ( Vector(..) )
import Math.Structure ( Field
                      , AdditiveMonoid, zero )
import Numeric.Natural ( Natural )

import HLinear.VVMatrix.Definition ( VVMatrix(..) )


data EchelonForm a = EchelonForm Natural Natural
                                 (Vector (Natural, Vector a))

 -- | A vector of rows, each offset by an integer
 --   Example: the vector with entries (1, vs) and (3, vs) would correspond to
 --   0 v v v v ...
 --   0 0 0 v v ...
toVVMatrix :: Field a
           => EchelonForm a -> VVMatrix a
toVVMatrix (EchelonForm nrs ncs rs) = VVMatrix nrs ncs rs'
  where
  rs' = (`V.map` rs) $ \(o, r) -> V.replicate (fromIntegral o) zero V.++ r

nmbRows :: EchelonForm a -> Natural
nmbRows (EchelonForm nrs _ _) = nrs

nmbCols :: EchelonForm a -> Natural
nmbCols (EchelonForm _ ncs _) = ncs

 -- | shift echelon form by a given number of columns
shift :: Natural -> EchelonForm a -> EchelonForm a
shift s (EchelonForm nrs ncs rs) =
  EchelonForm nrs (ncs+s) $ V.map (first (+s)) rs

concat :: EchelonForm a -> EchelonForm a -> EchelonForm a
concat (EchelonForm nrs ncs rs) e' =
  let
  EchelonForm nrs' ncs' rs' = shift (nrs-nrs') e'
  in
  EchelonForm nrs (ncs+ncs') $ rs V.++ rs'
