module HLinear.PLE.FoldUnfold.ReducedEchelonForm
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

import Debug.Trace


reduce
  :: ( Show a, DivisionRing a, DecidableZero a )
  => EchelonForm a -> (EchelonTransformation a, EchelonForm a)
reduce ef =
  let EchelonReduction et' _ ef' =
        V.foldl (*) firstReduction $
        V.unfoldr reduceLastPivot ef
  in  (et', ef')
  where
    firstReduction = EchelonReduction
                       (ET.identityET nrs)
                       (M.zeroMatrix nrs 0)
                       (EF.zeroEF 0 0)
    nrs = EF.nmbRows ef
    ncs = EF.nmbCols ef


data EchelonReduction a =
  EchelonReduction (EchelonTransformation a) (Matrix a) (EchelonForm a)
  deriving Show

instance DivisionRing a => MultiplicativeMagma (EchelonReduction a) where
  (EchelonReduction et m ef) * (EchelonReduction et' m' ef') =
    EchelonReduction
      (et'*et)
      (M.blockSumRows m' mTop)
      (EF.blockSumAtPivot ef' mBottom ef)
    where
      (mTop,mBottom) = M.splitAtCols (fromIntegral $ M.nmbRows m') (et'*.m)

instance DivisionRing a => MultiplicativeSemigroup (EchelonReduction a)


reduceLastPivot
  :: ( Show a, DivisionRing a, DecidableZero a )
  => EchelonForm a -> Maybe (EchelonReduction a, EchelonForm a)
reduceLastPivot ef@(EchelonForm nrs ncs rs)
  | nrs == 0 && ncs == 0 = Nothing
  | otherwise            = Just $
      case EF.splitAtPivot 0 ef of
        Nothing -> ( EchelonReduction
                       (ET.identityET nrs)
                       (Matrix 0 ncs V.empty)
                       ef
                   , EchelonForm 0 0 V.empty )
        Just (efLeft, efTopRight, efBottomRight) -> 
          ( EchelonReduction et efTopRight' efBottomRight
          , efLeft )
          where
            et = ET.singleton $ V.map negate $ M.headCols efTopRight 
            nrs'Z = fromIntegral $ EF.nmbRows efLeft
            ncs' = EF.nmbCols efBottomRight
            efBottomRightHead = M.Matrix 1 ncs' $ V.singleton $
                                  efBottomRight `EF.atRow` 0
            (efTopRight',_) = M.splitAtCols nrs'Z $
                              et *. M.blockSumCols efTopRight efBottomRightHead
