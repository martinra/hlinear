module HLinear.PLE.Hook.ReducedEchelonForm
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Permute as P
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.PLE.Hook.EchelonForm ( EchelonForm(..) )
import qualified HLinear.PLE.Hook.EchelonForm as EF
import qualified HLinear.PLE.Hook.EchelonForm.Row as EFR
import HLinear.PLE.Hook.EchelonTransformation ( EchelonTransformation(..) )
import qualified HLinear.PLE.Hook.EchelonTransformation as ET
import HLinear.Matrix ( zeroMatrix
                      , headRows, tailRows
                      )
import qualified HLinear.Matrix as M
import HLinear.Matrix.Conversion
import HLinear.Matrix.Definition ( Matrix(..) )


newtype ReducedEchelonForm a = Reduced (EchelonForm a)

reduce
  :: ( DivisionRing a, DecidableZero a )
  => EchelonForm a -> (EchelonTransformation a, ReducedEchelonForm a)
reduce ef =
  let EchelonReduction et' _ ef' =
        V.foldl (*) firstReduction $
        V.unfoldr reduceLastPivot ef
  in  (et', Reduced ef')
  where
    firstReduction = EchelonReduction
                       (ET.identityET nrs)
                       (M.zeroMatrix nrs 0)
                       (EF.zeroEF 0 0)
    nrs = EF.nmbRows ef
    ncs = EF.nmbCols ef


data EchelonReduction a =
  EchelonReduction (EchelonTransformation a) (Matrix a) (EchelonForm a)

instance DivisionRing a => MultiplicativeMagma (EchelonReduction a) where
  (EchelonReduction et m ef) * (EchelonReduction et' m' ef') =
    EchelonReduction
      (et'*et)
      (M.blockSumRows m' mTop)
      (EF.blockSumAtPivot ef' mBottom ef)
    where
      (mTop,mBottom) = M.splitAtRows (fromIntegral $ M.nmbRows m') (et'*.m)

instance DivisionRing a => MultiplicativeSemigroup (EchelonReduction a)


reduceLastPivot
  :: ( DivisionRing a, DecidableZero a )
  => EchelonForm a -> Maybe (EchelonReduction a, EchelonForm a)
reduceLastPivot ef@(EchelonForm nrs ncs rs) =
  go <$> EF.splitAtPivot 0 ef 
  where
    go (efLeft,efTopRight,efBottomRight) = 
      let et = ET.singleton $ V.map negate $ M.headRows efTopRight 
      in ( EchelonReduction et (et*.efTopRight) efBottomRight, efLeft )
      
