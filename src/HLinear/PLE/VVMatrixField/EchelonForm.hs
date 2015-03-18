module HLinear.PLE.VVMatrixField.EchelonForm
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

import HLinear.VVMatrix.Definition ( VVMatrix(..) )


data EchelonForm a = EchelonForm Int Int (Vector (Int, Vector a))

 -- | A vector of rows, each offset by an integer
 --   Example: the vector with entries (1, vs) and (3, vs) would correspond to
 --   0 v v v v ...
 --   0 0 0 v v ...
toVVMatrix :: Field a
                      => EchelonForm a -> VVMatrix a
toVVMatrix (EchelonForm nrs ncs rs) = VVMatrix nrs ncs rows 
  where
  rows = V.map (\(o, r) -> V.replicate o zero V.++ r) rs

nmbRows :: EchelonForm a -> Int
nmbRows (EchelonForm nrs _ _) = nrs

nmbCols :: EchelonForm a -> Int
nmbCols (EchelonForm _ ncs _) = ncs

 -- | shift echelon form by a given number of columns
shift :: Int -> EchelonForm a -> EchelonForm a
shift s (EchelonForm nrs ncs rs) =
  EchelonForm nrs (ncs+s) (V.map (first (+s)) rs)

concat :: EchelonForm a -> EchelonForm a -> EchelonForm a
concat (EchelonForm nrs ncs rs) e' =
  let
  EchelonForm nrs' ncs' rs' = shift (nrs-nrs') e'
  in
  EchelonForm nrs (ncs+ncs') $ rs V.++ rs'

