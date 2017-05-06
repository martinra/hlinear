module HLinear.Matrix.Naive
where

import qualified Prelude as P
import HLinear.Utility.Prelude hiding ( recip )

import qualified Data.Vector as V
import qualified Math.Structure as MS

import HLinear.Matrix.Algebra
import HLinear.Matrix.Definition


minor :: Int -> Int -> Matrix a -> Matrix a
minor ix jx (Matrix nrs ncs rs) = Matrix (nrs P.- 1) (ncs P.- 1) rs'
  where
    splitAt kx = second V.tail . V.splitAt kx 
    splitRows = V.unzip . fmap (splitAt ix)
    ((topLeft,topRight), (bottomLeft,bottomRight)) =
      (splitRows *** splitRows) $ splitAt ix rs
    rs' = (V.zipWith (<>) topLeft topRight) <> (V.zipWith (<>) bottomLeft bottomRight)

det :: Ring a => Matrix a -> a
det m@(Matrix nrs ncs rs)
  | nrs == ncs = foldl' (+) one $
                   (\f -> fmap f $ V.enumFromN 0 (fromIntegral nrs)) $ \ix ->
                     (if even ix then id else negate) $ det $ minor ix ix m
  | otherwise = undefined

recip
  :: ( Ring a, DecidableUnit a, MultiplicativeGroup (Unit a) )
  => Matrix a -> Matrix a
recip = fromJust . recipSafe

recipSafe
  :: ( Ring a, DecidableUnit a, MultiplicativeGroup (Unit a) )
  => Matrix a -> Maybe (Matrix a)
recipSafe m@(Matrix nrs ncs rs)
  | nrs /= ncs = Nothing
  | otherwise = do
      recipDet <- fromUnit <$> MS.recip <$> toUnitSafe (det m)
      return $ Matrix nrs ncs $
        V.generate (fromIntegral nrs) $ \ix ->
          V.generate (fromIntegral ncs) $ \jx ->
            (if even (ix+jx) then id else negate) $ recipDet * det (minor jx ix m)
