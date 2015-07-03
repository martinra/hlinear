{-# LANGUAGE
    StandaloneDeriving
  #-}

module HLinear.PLE.Hook.EchelonForm.Basic
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
import Safe ( atMay )

import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.PLE.Hook.EchelonForm.Definition as EF
import HLinear.PLE.Hook.EchelonForm.Row as EFR


-- properties

offset :: EchelonForm a -> Natural
offset (EchelonForm nrs _ rs)
  | V.null rs = nrs
  | otherwise = EFR.offset $ V.head rs

pivotIxs :: DecidableZero a => EchelonForm a -> [(Int,Int)]
pivotIxs (EchelonForm nrs ncs rs) =
  go [] (pred $ V.length rs) (fromIntegral ncs)
  where
    go ps ix jx
      | ix < 0 || jx < 0 = ps
      | otherwise = case EFR.pivotIx (rs V.! ix) of
                      Nothing  -> go ps (pred ix) jx
                      Just jx' -> go ((ix,jx'):ps) (pred ix) (pred jx')
      
-- access and conversion

(!) :: AdditiveMonoid a => EchelonForm a -> Int -> Vector a
(!) (EchelonForm nrs ncs rs) ix
  | ix >= ncsZ = error "EchelonForm.(!): out of range"
  | otherwise  = V.map (EFR.! ix) rs
                 V.++
                 V.replicate (nrsZ - V.length rs) zero
    where
      nrsZ = fromIntegral nrs
      ncsZ = fromIntegral ncs

-- Eq and Show

deriving instance Show a => Show (EchelonForm a)

instance ( Eq a, DecidableZero a ) => Eq (EchelonForm a) where
  (EchelonForm nrs ncs rs) == (EchelonForm nrs' ncs' rs') =
    nrs == nrs' && ncs == ncs' && rs == rs'

-- conversion

toMatrix :: AdditiveMonoid a => EchelonForm a -> Matrix a
toMatrix (EchelonForm nrs ncs rs) = Matrix nrs ncs rs'
  where
    hrs = V.map EFR.row rs
    rs' = V.map EFR.toVector rs V.++ zeros
    zeros = V.replicate ((fromIntegral nrs) - V.length rs)
                        (V.replicate (fromIntegral ncs) zero)

-- creation

singletonLeadingOne :: MultiplicativeMonoid a
                     => Natural -> Natural -> Vector a
                     -> EchelonForm a
singletonLeadingOne nrs o v = singleton nrs o $ one `V.cons` v

singleton :: Natural -> Natural -> Vector a
           -> EchelonForm a
singleton nrs o v = EchelonForm nrs nv $
                      V.singleton $ EchelonFormRow o v
  where
    nv = fromIntegral $ V.length v

zeroEF :: Natural -> Natural -> EchelonForm a
zeroEF nrs ncs = EchelonForm nrs ncs V.empty

-- submatrices

splitAt :: Int -> EchelonForm a -> (EchelonForm a, EchelonForm a)
splitAt ix (EchelonForm nrs ncs rs) =
  ( EchelonForm nrs ncs rsTop
  , EchelonForm nrs' ncs rsBottom
  )
  where
    nrsZ = fromIntegral nrs
    ixB = min nrsZ $ max 0 ix
    (rsTop,rsBottom) = V.splitAt ix rs
    nrs' = fromIntegral $ nrsZ - ixB

splitAtPivot
  :: ( AdditiveMonoid a, DecidableZero a )
  => Int -> EchelonForm a
  -> Maybe (EchelonForm a, Matrix a, EchelonForm a)
splitAtPivot ix ef@(EchelonForm nrs ncs rs)
  = go <$> pivotIxs ef `atMay` ix
  where
    go pivotIx@(pivotRow,pivotCol) =
      ( EchelonForm nrs pivotColN rsLeft
      , Matrix nrs' pivotColN $ V.map EFR.toVector rsTopRight
      , EchelonForm nrs' ncs' rsBottomRight
      )
      where
      (rsLeft,rsRight) = V.unzip $ V.map (EFR.splitAt pivotCol) rs
      (rsTopRight,rsBottomRight) = V.splitAt pivotRow rsRight
  
      nrsZ = fromIntegral nrs
      ncsZ = fromIntegral ncs
      nrs' = fromIntegral $ nrsZ - pivotRow
      ncs' = fromIntegral $ ncsZ - pivotCol
      pivotColN = fromIntegral pivotCol

-- block sums

blockSumAtPivot
  :: EchelonForm a -> Matrix a -> EchelonForm a
  -> EchelonForm a
blockSumAtPivot
  (EchelonForm nrs ncs rs)
  (Matrix nrs' ncs' rs')
  (EchelonForm nrs'' ncs'' rs'')
  | nrs /= nrs'   = error "EchelonForm.sumRow: incompatible number of rows"
  | ncs' /= ncs'' = error "EchelonForm.sumRow: incompatible number of columns"
  | otherwise  = EchelonForm (nrs+nrs'') (ncs+ncs') $
                   V.zipWith EFR.sumRow rs rs'
                   V.++
                   V.map (EFR.setLength $ fromIntegral $ nrs+nrs'') rs''
