module HLinear.Hook.EchelonForm.Basic
where

import Prelude ()
import HLinear.Utility.Prelude hiding ( zero )

import qualified Data.Vector as V
import qualified Math.Structure as MS

import HLinear.Hook.EchelonForm.Definition as EF
import HLinear.Hook.EchelonForm.Row ( EchelonFormRow(..) )
import HLinear.Matrix.Definition ( Matrix(..), IsMatrix(..) )
import qualified HLinear.Hook.EchelonForm.Row as EFR


--------------------------------------------------------------------------------
-- Eq, Show, and NFData
--------------------------------------------------------------------------------

deriving instance Show a => Show (EchelonForm a)

instance ( Eq a, DecidableZero a ) => Eq (EchelonForm a) where
  (EchelonForm nrs ncs rs) == (EchelonForm nrs' ncs' rs') =
    nrs == nrs' && ncs == ncs' && rs == rs'

instance NFData a => NFData (EchelonForm a) where
  rnf (EchelonForm nrs ncs rs) =
    seq (rnf nrs) $
    seq (rnf ncs) $
    seq (V.map rnf rs) ()

--------------------------------------------------------------------------------
-- rows and columns
--------------------------------------------------------------------------------

instance HasNmbRows (EchelonForm a) where
  nmbRows (EchelonForm nrs _ _) = nrs

instance HasNmbCols (EchelonForm a) where
  nmbCols (EchelonForm _ ncs _) = ncs

--------------------------------------------------------------------------------
-- attributes
--------------------------------------------------------------------------------

offset :: EchelonForm a -> Natural
offset (EchelonForm nrs _ rs)
  | V.null rs = nrs
  | otherwise = EFR.offset $ V.head rs
                      
at :: AdditiveMonoid a => EchelonForm a -> Int -> Int -> a
at (EchelonForm nrs ncs rs) ix jx
  | ix >= nrsZ  = error  "EchelonForm.at: row index out of range"
  | otherwise   =
      let getFromRow (EchelonFormRow o r) =
            fromMaybe MS.zero $ r V.!? (jx - fromIntegral o)
      in  maybe MS.zero getFromRow $ rs V.!? ix
  where
    nrsZ = fromIntegral nrs

atRow :: AdditiveMonoid a => EchelonForm a -> Int -> Vector a
atRow (EchelonForm nrs ncs rs) ix
  | ix >= nrsZ  = error  "EchelonForm.atRow: out of range"
  | otherwise   = maybe (V.replicate ncsZ MS.zero) EFR.toVector $ rs V.!? ix
  where
    nrsZ = fromIntegral nrs
    ncsZ = fromIntegral ncs

atCol :: AdditiveMonoid a => EchelonForm a -> Int -> Vector a
atCol (EchelonForm nrs ncs rs) ix
  | ix >= ncsZ = error "EchelonForm.atCol: out of range"
  | otherwise  = V.map (EFR.! ix) rs
                 V.++
                 V.replicate (nrsZ - V.length rs) MS.zero
    where
      nrsZ = fromIntegral nrs
      ncsZ = fromIntegral ncs

--------------------------------------------------------------------------------
-- container
--------------------------------------------------------------------------------

instance Functor EchelonForm where
  fmap = fmapDefault

instance Foldable EchelonForm where
  foldMap = foldMapDefault

instance Traversable EchelonForm where
  traverse f (EchelonForm nrs ncs rs) = EchelonForm nrs ncs <$> traverse (traverse f) rs

--------------------------------------------------------------------------------
-- conversion
--------------------------------------------------------------------------------

instance AdditiveMonoid a => IsMatrix (EchelonForm a) a where
  toMatrix (EchelonForm nrs ncs rs) = Matrix nrs ncs rs'
    where
      hrs = V.map EFR.row rs
      rs' = V.map EFR.toVector rs V.++ zeros
      zeros = V.replicate (fromIntegral nrs - V.length rs)
                          (V.replicate (fromIntegral ncs) MS.zero)

--------------------------------------------------------------------------------
-- creation
--------------------------------------------------------------------------------

zero :: Natural -> Natural -> EchelonForm a
zero nrs ncs = EchelonForm nrs ncs V.empty

singleton
  :: Natural -> Vector a
  -> EchelonForm a
singleton nrs v = EchelonForm nrs nv $ V.singleton $ EFR.singleton v
  where
    nv = fromIntegral $ V.length v

singletonLeadingOne
  :: MultiplicativeMonoid a
  => Natural -> Vector a
  -> EchelonForm a
singletonLeadingOne nrs v = singleton nrs $ one `V.cons` v

--------------------------------------------------------------------------------
-- submatrices
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- block sums
--------------------------------------------------------------------------------

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
                V.replicate ncs'Z MS.zero

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
