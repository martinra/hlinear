{-# LANGUAGE
    StandaloneDeriving
  #-}

module HLinear.PLE.Hook.EchelonForm.Basic
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      , splitAt
                      )

import Control.DeepSeq ( NFData(..) )
import Data.Maybe
import Data.Vector ( Vector(..) )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ( Natural )
import Safe ( atMay )

import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.PLE.Hook.EchelonForm.Definition as EF
import HLinear.PLE.Hook.EchelonForm.PivotStructure ( rank )
import qualified HLinear.PLE.Hook.EchelonForm.Row as EFR
import HLinear.PLE.Hook.EchelonForm.Row ( EchelonFormRow(..) )

-- properties

offset :: EchelonForm a -> Natural
offset (EchelonForm nrs _ rs)
  | V.null rs = nrs
  | otherwise = EFR.offset $ V.head rs
                      
-- access and conversion

atRow :: AdditiveMonoid a => EchelonForm a -> Int -> Vector a
atRow (EchelonForm nrs ncs rs) ix
  | ix >= nrsZ  = error  "EchelonForm.atRow: out of range"
  | otherwise   = maybe (V.replicate ncsZ zero) EFR.toVector $ rs V.!? ix
  where
    nrsZ = fromIntegral nrs
    ncsZ = fromIntegral ncs

atCol :: AdditiveMonoid a => EchelonForm a -> Int -> Vector a
atCol (EchelonForm nrs ncs rs) ix
  | ix >= ncsZ = error "EchelonForm.atCol: out of range"
  | otherwise  = V.map (EFR.! ix) rs
                 V.++
                 V.replicate (nrsZ - V.length rs) zero
    where
      nrsZ = fromIntegral nrs
      ncsZ = fromIntegral ncs

-- Eq, Show, and NFData

deriving instance Show a => Show (EchelonForm a)

instance ( Eq a, DecidableZero a ) => Eq (EchelonForm a) where
  (EchelonForm nrs ncs rs) == (EchelonForm nrs' ncs' rs') =
    nrs == nrs' && ncs == ncs' && rs == rs'

instance NFData a => NFData (EchelonForm a) where
  rnf (EchelonForm nrs ncs rs) =
    seq (rnf nrs) $
    seq (rnf ncs) $
    seq (V.map rnf rs) ()

-- conversion

toMatrix :: AdditiveMonoid a => EchelonForm a -> Matrix a
toMatrix (EchelonForm nrs ncs rs) = Matrix nrs ncs rs'
  where
    hrs = V.map EFR.row rs
    rs' = V.map EFR.toVector rs V.++ zeros
    zeros = V.replicate (fromIntegral nrs - V.length rs)
                        (V.replicate (fromIntegral ncs) zero)

-- creation

zeroEF :: Natural -> Natural -> EchelonForm a
zeroEF nrs ncs = EchelonForm nrs ncs V.empty

singletonLeadingOne
  :: MultiplicativeMonoid a
  => Natural -> Natural -> Vector a
  -> EchelonForm a
singletonLeadingOne nrs o v = singleton nrs o $ one `V.cons` v

singleton
  :: Natural -> Natural -> Vector a
  -> EchelonForm a
singleton nrs o v = EchelonForm nrs nv $
                      V.singleton $ EchelonFormRow o v
  where
    nv = fromIntegral $ V.length v

-- submatrices

splitAt :: Int -> EchelonForm a -> (EchelonForm a, EchelonForm a)
splitAt ix (EchelonForm nrs ncs rs) =
  ( EchelonForm ixBN ncs rsTop
  , EchelonForm nrs' ncs rsBottom
  )
  where
    nrsZ = fromIntegral nrs
    ixB = min nrsZ $ max 0 ix
    ixBN = fromIntegral ixB
    nrs' = fromIntegral $ nrsZ - ixB

    (rsTop,rsBottom) = V.splitAt ix rs

truncateAtRank
 :: DecidableZero a
 => EchelonForm a -> EchelonForm a
truncateAtRank e = fst $ splitAt (fromIntegral $ rank e) e

splitAtHook
  :: ( AdditiveMonoid a, DecidableZero a )
  => (Int,Int) -> EchelonForm a
  -> (EchelonForm a, Matrix a, EchelonForm a)
splitAtHook (pivotRow,pivotCol) ef@(EchelonForm nrs ncs rs)
  | pivotRow < 0 || pivotCol < 0 =
      splitAtHook (max 0 pivotRow, max 0 pivotCol) ef
  | pivotRow >= nrsZ || pivotCol >= ncsZ =
      splitAtHook (min nrsZ pivotRow, min ncsZ pivotCol) ef
  | otherwise =
      ( EchelonForm pivotRowN pivotColN rsLeft
      , Matrix pivotRowN ncs' $ V.map EFR.toVector rsTopRight
      , EchelonForm nrs' ncs' rsBottomRight
      )
    where
    (rsLeft,rsRight) = V.unzip $ V.map (EFR.splitAt pivotCol) rs
    (rsTopRight,rsBottomRight) = V.splitAt pivotRow rsRight
  
    nrsZ = fromIntegral nrs
    ncsZ = fromIntegral ncs
    nrs' = fromIntegral $ nrsZ - pivotRow
    ncs' = fromIntegral $ ncsZ - pivotCol
    pivotRowN = fromIntegral pivotRow
    pivotColN = fromIntegral pivotCol

-- block sums

blockSum
  :: AdditiveMonoid a
  => EchelonForm a -> Matrix a
  -> EchelonForm a
blockSum
  (EchelonForm nrs ncs rs)
  (Matrix nrs' ncs' rs')
  | nrs < nrs'   = error "EchelonForm.blockSum: incompatible number of rows"
  | otherwise  = EchelonForm nrs (ncs+ncs') $
                   V.zipWith EFR.sumRow rs1 rs'
                   V.++
                   V.zipWith EFR.sumRow rs2 zerors
      where
      nrs'Z = fromIntegral nrs'
      ncs'Z = fromIntegral ncs'

      (rs1,rs2) = V.splitAt nrs'Z rs
      zerors = V.replicate (V.length rs2) $
                V.replicate ncs'Z zero

blockSumHook
  :: EchelonForm a -> Matrix a -> EchelonForm a
  -> EchelonForm a
blockSumHook
  (EchelonForm nrs ncs rs)
  (Matrix nrs' ncs' rs')
  (EchelonForm nrs'' ncs'' rs'')
  | nrs /= nrs'   = error "EchelonForm.blockSumAtPivot: incompatible number of rows"
  | ncs' /= ncs'' = error "EchelonForm.blockSumAtPivot: incompatible number of columns"
  | otherwise  = EchelonForm (nrs+nrs'') (ncs+ncs') $
                   V.zipWith EFR.sumRow rs rs'
                   V.++
                   V.map (EFR.setLength $ fromIntegral $ ncs+ncs'') rs''
