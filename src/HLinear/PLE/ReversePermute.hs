module HLinear.PLE.ReversePermute
where

import qualified Data.Vector as V
import Data.Vector ( Vector(..) )
import Data.Permute


newtype ReversePermute = ReversePermute Permute

concat :: ReversePermute -> ReversePermute -> ReversePermute
concat (ReversePermute p) (ReversePermute p') =
  ReversePermute $ swapsPermute n $ swaps p ++ swaps p'
  where
  n = max (size p) (size p')

reversePermute :: Int -> ReversePermute
reversePermute n = ReversePermute $ permute n

swapReversePermute :: Int -> (Int,Int) -> ReversePermute
swapReversePermute n (a,b) = ReversePermute $ swapsPermute n [(a,b)]

apply :: ReversePermute -> Int -> Vector a -> Vector a
apply (ReversePermute p) nrs rs =
  V.backpermute rs $ V.generate nrs ((nrs-1-) . (p `at`))

