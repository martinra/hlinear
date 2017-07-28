module HLinear.Hook.EchelonForm.Basic
where

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
    seq (fmap rnf rs) ()

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

offset :: EchelonForm a -> Int
offset (EchelonForm nrs _ rs)
  | V.null rs = nrs
  | otherwise = EFR.offset $ V.head rs
                      
at :: AdditiveMonoid a => EchelonForm a -> Int -> Int -> a
at (EchelonForm nrs ncs rs) ix jx
  | ix >= nrs = error  "EchelonForm.at: row index out of range"
  | otherwise =
      let getFromRow (EchelonFormRow o r) =
            fromMaybe MS.zero $ r V.!? (jx-o)
      in  maybe MS.zero getFromRow $ rs V.!? ix

atRow :: AdditiveMonoid a => EchelonForm a -> Int -> Vector a
atRow (EchelonForm nrs ncs rs) ix
  | ix >= nrs = error  "EchelonForm.atRow: out of range"
  | otherwise = maybe (V.replicate ncs MS.zero) EFR.toVector $ rs V.!? ix

atCol :: AdditiveMonoid a => EchelonForm a -> Int -> Vector a
atCol (EchelonForm nrs ncs rs) ix
  | ix >= ncs = error "EchelonForm.atCol: out of range"
  | otherwise = fmap (EFR.! ix) rs
                 <>
                 V.replicate (nrs - V.length rs) MS.zero

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
      hrs = fmap EFR.row rs
      rs' = fmap EFR.toVector rs <> zeros
      zeros = V.replicate (nrs - V.length rs) (V.replicate ncs MS.zero)

--------------------------------------------------------------------------------
-- creation
--------------------------------------------------------------------------------

zero :: Int -> Int -> EchelonForm a
zero nrs ncs
  | nrs >= 0 && ncs >= 0 = EchelonForm nrs ncs V.empty
  | nrs < 0 = error "EchelonForm.zero: negative nrs"
  | ncs < 0 = error "EchelonForm.zero: negative ncs"

singleton :: Int -> Vector a -> EchelonForm a
singleton nrs v
  | nrs >= 0 = EchelonForm nrs (V.length v) $ V.singleton $ EFR.singleton v
  | nrs < 0 = error "EchelonForm.singleton: negative nrs"


singletonLeadingOne
  :: MultiplicativeMonoid a
  => Int -> Vector a
  -> EchelonForm a
singletonLeadingOne nrs v
  | nrs >= 0 = singleton nrs $ one `V.cons` v
  | nrs < 0  = error "EchelonForm.singletonLeadingOne: negative nrs"

--------------------------------------------------------------------------------
-- normalization
--------------------------------------------------------------------------------

normalize
  :: ( DivisionRing a, DecidableZero a )
  => EchelonForm a -> EchelonForm a
normalize (EchelonForm nrs ncs rs) = EchelonForm nrs ncs $ fmap EFR.normalize rs

--------------------------------------------------------------------------------
-- submatrices
--------------------------------------------------------------------------------

splitAt :: Int -> EchelonForm a -> (EchelonForm a, EchelonForm a)
splitAt ix (EchelonForm nrs ncs rs) =
  ( EchelonForm ixB ncs rsTop
  , EchelonForm nrs' ncs rsBottom
  )
  where
    ixB = min nrs $ max 0 ix
    nrs' = nrs - ixB

    (rsTop,rsBottom) = V.splitAt ix rs

splitAtHook
  :: ( AdditiveMonoid a, DecidableZero a )
  => (Int,Int) -> EchelonForm a
  -> (EchelonForm a, Matrix a, EchelonForm a)
splitAtHook (pivotRow,pivotCol) ef@(EchelonForm nrs ncs rs)
  | pivotRow < 0 || pivotCol < 0 =
      splitAtHook (max 0 pivotRow, max 0 pivotCol) ef
  | pivotRow >= nrs || pivotCol >= ncs =
      splitAtHook (min nrs pivotRow, min ncs pivotCol) ef
  | otherwise =
      ( EchelonForm pivotRow pivotCol rsLeft
      , Matrix pivotRow ncs' $ fmap EFR.toVector rsTopRight
      , EchelonForm nrs' ncs' rsBottomRight
      )
    where
    (rsLeft,rsRight) = V.unzip $ fmap (EFR.splitAt pivotCol) rs
    (rsTopRight,rsBottomRight) = V.splitAt pivotRow rsRight
  
    nrs' = nrs - pivotRow
    ncs' = ncs - pivotCol

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
                   <>
                   V.zipWith EFR.sumRow rs2 zerors
      where
        (rs1,rs2) = V.splitAt nrs' rs
        zerors = V.replicate (V.length rs2) $
                  V.replicate ncs' MS.zero

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
                   <>
                   fmap (EFR.setLength $ ncs+ncs'') rs''
