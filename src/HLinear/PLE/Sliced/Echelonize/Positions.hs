module HLinear.PLE.Sliced.Echelonize.Positions
where

import Control.Arrow ( first )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Numeric.Natural

import HLinear.PLE.Sliced.Echelonize.Definition


slicingPositions
  :: PLEDecompositionSlicedParameters
  -> Natural -> Vector (Int, Int)
slicingPositions _ 0 = V.empty

slicingPositions
  ( PLEDecompositionSlicedParameters
    SlicingUnBalanced paramSizeNmb
  ) ncs
  = bigSlices V.++ smallSlices
  where
    ncsZ = fromIntegral ncs

    nmbBigSlices = fromIntegral $
      case paramSizeNmb of
        SlicingSize paramSz -> ncs `div` paramSz
        SlicingNmb paramNmb -> paramNmb

    sizeBigSlice = fromIntegral $
      case paramSizeNmb of
        SlicingSize paramSz -> paramSz
        SlicingNmb paramNmb -> ncs `div` paramNmb

    sizeSmallSlice = fromIntegral $
      case paramSizeNmb of
        SlicingSize paramSz -> ncs `mod` paramSz
        SlicingNmb paramNmb -> ncs `mod` paramNmb

    bigSlices = if sizeBigSlice == 0
                then V.empty
                else V.iterateN nmbBigSlices
                       (first (+sizeBigSlice)) (0,sizeBigSlice)
    smallSlices = if sizeSmallSlice == 0
                  then V.empty
                  else V.singleton ( sizeBigSlice * nmbBigSlices
                                   , sizeSmallSlice )

slicingPositions
  ( PLEDecompositionSlicedParameters
    SlicingBalanced (SlicingSize paramSz)
  ) ncs
  = if remainder == 0
    then normalSlices
    else normalSlices
         `V.snoc`
         ( ncsZ - remainder, remainder )
  where
    ncsZ = fromIntegral ncs
    sz = fromIntegral paramSz

    (n,r) = ncsZ `divMod` sz

    (nmbBigSlices, nmbSmallSlices, remainder) =
      if sz-1 - r > n
      then (n,0,r)
      else (n - (sz-1) + r, 1 + sz-1 - r, 0)
    nmbBothSlices = nmbBigSlices + nmbSmallSlices

    normalSlices =
      if nmbBothSlices == 0
      then V.empty
      else slicingPositions
             ( PLEDecompositionSlicedParameters
               SlicingBalanced (SlicingNmb $ fromIntegral nmbBothSlices)
             )
             ( fromIntegral $ ncsZ - remainder )


slicingPositions
  ( PLEDecompositionSlicedParameters
    SlicingBalanced (SlicingNmb nmbSlices)
  ) ncs
  = bigSlices V.++ smallSlices
  where
    nmbSlicesZ = fromIntegral nmbSlices

    nmbBigSlices = fromIntegral $ ncs `mod` nmbSlices
    nmbSmallSlices = nmbSlicesZ - nmbBigSlices

    sizeSmallSlice = fromIntegral $ ncs `div` nmbSlices
    sizeBigSlice = succ sizeSmallSlice

    bigSlices =
      if sizeBigSlice == 0
      then V.empty
      else V.iterateN nmbBigSlices
             (first (+sizeBigSlice)) (0,sizeBigSlice)
    smallSlices =
      if sizeSmallSlice == 0
      then V.empty
      else V.iterateN nmbSmallSlices
             ( first (+sizeSmallSlice) )
             ( nmbBigSlices * sizeBigSlice
             , sizeSmallSlice
             )
