module HLinear.PLE.Hook.EchelonForm
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import Control.Arrow ( first )
import qualified Data.Vector as V
import Data.Vector ( Vector(..) )
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.BRMatrix.Definition ( BRMatrix(..) )
import HLinear.BRMatrix.RVector


-- represents a matrix of given size that has echelon normal form
data EchelonForm a = EchelonForm Natural Natural (Vector (EchelonFormRow a))
data EchelonFormRow a = EchelonFormRow Natural (Vector a)

 -- | A vector of rows, each set off by a number of zeros
 --   The offset is assumed to be strictly increasing
 --   Example: the vector with entries (1, vs) and (3, vs) would correspond to
 --   0 v v v v ...
 --   0 0 0 v v ...
toMatrix :: DivisionRing a
           => EchelonForm a -> BRMatrix a
toMatrix (EchelonForm nrs ncs rs) = BRMatrix nrs ncs rs'
  where
  rs' = RVector $ (`V.map` rs) $ \(EchelonFormRow s r) ->
          RVector $ V.replicate (fromIntegral s) zero V.++ r

nmbRows :: EchelonForm a -> Natural
nmbRows (EchelonForm nrs _ _) = nrs

nmbCols :: EchelonForm a -> Natural
nmbCols (EchelonForm _ ncs _) = ncs

offset :: EchelonForm a -> Natural
offset (EchelonForm _ _ v)
  | V.null v  = 0
  | otherwise = (\(EchelonFormRow s _) -> s) $ V.last v

 -- | shift echelon form by a given number of columns
shift :: Natural -> EchelonForm a -> EchelonForm a
shift s (EchelonForm nrs ncs rs) =
  EchelonForm nrs (ncs+s) $ V.map go rs
    where
    go (EchelonFormRow s' r) = EchelonFormRow (s'+s) r

-- This can lead to inconsitent data structures. We assume that
-- the number of columns in the first argument equals the number of columns in
-- the second one + the offset of the first argument
instance MultiplicativeMagma (EchelonForm a) where
  e@(EchelonForm nrs ncs rs) * e' =
    let EchelonForm _ _ rs' = shift (offset e) e'
    in EchelonForm nrs ncs $ rs V.++ rs'

instance MultiplicativeSemigroup (EchelonForm a) where
