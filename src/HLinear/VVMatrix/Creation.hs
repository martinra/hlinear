module HLinear.VVMatrix.Creation
where

import Data.Maybe
import qualified Data.Vector as V
import Data.Vector ( Vector )
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.VVMatrix.Definition ( VVMatrix(..) )


-- toVectors is defined in module Force
fromVectors :: Vector (Vector a) -> VVMatrix a
fromVectors rs =
  -- todo: introduce unsafe version of fromVectors'
  fromVectors' nrs ncs rs
  where
  nrs = fromIntegral $ V.length rs
  ncs = if nrs == 0 then 0 else fromIntegral $ V.length (V.head rs)

fromVectors' :: Natural -> Natural -> Vector (Vector a) -> VVMatrix a
fromVectors' nrs ncs rs
  | nrs /= fromIntegral (V.length rs) =
      error $ "HLinear.VVMatrix fromVectors': " ++
              "number of rows incorrect"
  | any ((/=ncs) . fromIntegral . V.length) rs =
      error $ "HLinear.VVMatrix fromVectors': " ++
              "rows must have the same length"
  | otherwise = VVMatrix nrs ncs rs


-- toLists is defined in module Force
fromLists :: [[a]] -> VVMatrix a
fromLists = fromVectors . V.map V.fromList . V.fromList

fromLists' :: Natural -> Natural -> [[a]] -> VVMatrix a
fromLists' nrs ncs = fromVectors' nrs ncs . V.map V.fromList . V.fromList


zeroMatrix :: AdditiveMonoid a
           => Natural -> Natural -> VVMatrix a
zeroMatrix nrs ncs = VVMatrix nrs ncs $
                       V.replicate (fromIntegral nrs) $
                       V.replicate (fromIntegral ncs) zero

diagonalMatrix :: AdditiveMonoid a
               => Vector a -> VVMatrix a
diagonalMatrix ds = VVMatrix (fromIntegral nrs) (fromIntegral nrs) $
                      (`V.imap` ds) $ \ix d ->
                      V.generate nrs $ \jx ->
                        if ix==jx then d else zero
  where
  nrs = V.length ds

identityMatrix :: ( AdditiveMonoid a, MultiplicativeMonoid a )
               => Natural -> VVMatrix a
identityMatrix = diagonalMatrix . (`V.replicate` one) . fromIntegral


zeroMatrix' :: Maybe Natural -> Maybe Natural -> VVMatrix a
zeroMatrix' = Zero

oneMatrix' :: Maybe Natural -> a -> VVMatrix a
oneMatrix' = One
