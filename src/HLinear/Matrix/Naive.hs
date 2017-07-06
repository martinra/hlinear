module HLinear.Matrix.Naive
where

import HLinear.Utility.Prelude hiding ( recip )

import qualified Data.Vector as V
import qualified Math.Structure as MS

import HLinear.Matrix.Algebra
import HLinear.Matrix.Definition


minor :: Int -> Int -> Matrix a -> Matrix a
minor ix jx (Matrix nrs ncs rs) = Matrix (nrs-1) (ncs-1) rs'
  where
    splitAt kx = second V.tail . V.splitAt kx 
    splitRows = V.unzip . fmap (splitAt jx)
    ((topLeft,topRight), (bottomLeft,bottomRight)) =
      (splitRows *** splitRows) $ splitAt ix rs
    rs' = (V.zipWith (<>) topLeft topRight) <> (V.zipWith (<>) bottomLeft bottomRight)

det :: Ring a => Matrix a -> a
det m@(Matrix nrs ncs rs)
  | nrs /= ncs = error "Matrix.det only defined for square matrices"
  | nrs == 0   = one
  | nrs == 1   = rs V.! 0 V.! 0
  | otherwise  = foldl' (+) zero $
                   V.generate nrs $ \ix ->
                     ((rs V.! ix V.! 0) *) $
                     (if even ix then id else negate) $
                     det $ minor ix 0 m

recip
  :: ( Ring a, DecidableUnit a, MultiplicativeGroup (Unit a) )
  => Matrix a -> Matrix a
recip = fromJust . recipSafe

recipSafe
  :: ( Ring a, DecidableUnit a, MultiplicativeGroup (Unit a) )
  => Matrix a -> Maybe (Matrix a)
recipSafe m@(Matrix nrs ncs rs)
  | nrs /= ncs = Nothing
  | otherwise  = do
      recipDet <- fromUnit <$> MS.recip <$> toUnitSafe (det m)
      return $ Matrix nrs ncs $
        V.generate nrs $ \ix ->
          V.generate ncs $ \jx ->
            (if even (ix+jx) then id else negate) $ recipDet * det (minor jx ix m)
