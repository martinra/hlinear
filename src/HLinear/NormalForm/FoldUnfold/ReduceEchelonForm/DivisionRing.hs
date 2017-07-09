module HLinear.NormalForm.FoldUnfold.ReduceEchelonForm.DivisionRing
where

import qualified Prelude as P
import HLinear.Utility.Prelude

import Data.Sequence ( ViewR(..), viewr )
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Permute as P

import HLinear.Hook.ERHook ( ERHook(..) )
import HLinear.Hook.EchelonForm ( EchelonForm(..), PivotStructure(..) )
import HLinear.Hook.EchelonTransformation ( EchelonTransformation(..) )
import HLinear.Hook.PLEHook ( PLEHook(..), PLUEHook(..), UEHook(..) )
import HLinear.Matrix.Block ( headRows, tailRows )
import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.NormalForm.PLE ( HasPLE )
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Hook.EchelonForm.Row as EFR
import qualified HLinear.Hook.EchelonTransformation as ET
import qualified HLinear.Matrix.Basic as M
import qualified HLinear.NormalForm.PLE as PLE


{-# INLINABLE reduceEchelonForm #-}
reduceEchelonForm
  :: ( DivisionRing a, DecidableZero a )
  => EchelonForm a -> UEHook a
reduceEchelonForm ef =
  case reduceLastPivot (ef, EF.pivotStructure ef) of
    Nothing -> UEHook (ET.one nrs) (EF.zero nrs ncs)
    Just (er, efp') ->
      let ERHook et' _ ef' =
            V.foldl (*) er $ V.unfoldr reduceLastPivot efp'
      in  UEHook (ET.fitSize nrs et') ef'
  where
    nrs = nmbRows ef
    ncs = nmbCols ef

{-# INLINABLE reduceLastPivot #-}
reduceLastPivot
  :: ( DivisionRing a, DecidableZero a )
  => (EchelonForm a, PivotStructure)
  -> Maybe (ERHook a, (EchelonForm a, PivotStructure))
reduceLastPivot ( ef@(EchelonForm nrs ncs _), PivotStructure pivots )
  | nrs == 0 && ncs == 0 = Nothing
  | pivots' :> pivotRC <- viewr pivots = Just $
      let (efLeft, Matrix nrs' ncs' efTopRight, efBottomRight) =
            EF.splitAtHook pivotRC ef

          pivotRow = efBottomRight `EF.atRow` 0
          -- since we reduce EchelonForms over a division ring,
          -- the pivot entry is one
          pivotTop = fmap V.head efTopRight 
          pivotTopNormalization = pivotTop

          et = ET.singleton $ fmap negate pivotTopNormalization

          efTopRight' =
            (\f -> V.zipWith f efTopRight pivotTopNormalization) $ \r h ->
              V.zipWith (\re pe -> re - h*pe) r pivotRow

      in  ( ERHook et (Matrix nrs' ncs' efTopRight') efBottomRight
          , (efLeft, PivotStructure pivots')
          )
  | otherwise = Just
      ( ERHook (ET.one nrs) (M.zero 0 ncs) ef 
      , (EF.zero 0 0, PivotStructure S.empty)
      )
