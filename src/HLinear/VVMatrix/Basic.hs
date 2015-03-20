module HLinear.VVMatrix.Basic
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Applicative ( (<$>), (<*>) )
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector ( Vector )
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.VVMatrix.Creation
import HLinear.VVMatrix.Definition ( VVMatrix(..) )
import HLinear.VVMatrix.Utils


nmbRows :: VVMatrix a -> Maybe Natural
nmbRows (Zero nrs _) = nrs
nmbRows (One nrs _)  = nrs
nmbRows (VVMatrix nrs _ _) = Just nrs

nmbCols :: VVMatrix a -> Maybe Natural
nmbCols (Zero _ ncs) = ncs
nmbCols (One ncs _)  = ncs
nmbCols (VVMatrix _ ncs _) = Just ncs


transpose :: VVMatrix a -> VVMatrix a
transpose (Zero nrs ncs) = Zero ncs nrs
transpose m@(One _ _) = m
transpose (VVMatrix nrs ncs rs) =
  VVMatrix ncs nrs $
    V.generate (fromIntegral ncs) $ \ix ->
    V.generate (fromIntegral nrs) $ \jx -> rs V.! jx V.! ix


instance   ( DecidableZero a, DecidableOne a, Eq a)
         =>  Eq (VVMatrix a) where
  (VVMatrix nrs ncs rs) == (VVMatrix nrs' ncs' rs') =
    nrs == nrs' && ncs == ncs' && rs == rs'

  (Zero nrs ncs) == (Zero nrs' ncs') =
    nrs == nrs' && ncs == ncs'
  (Zero nrs ncs) == (One nrs' a') =
    nrs == nrs' && ncs == nrs' && isZero a'
  (One nrs a) == (Zero nrs' ncs') =
    nrs == nrs' && nrs == ncs' && isZero a
  (Zero nrs ncs) == (VVMatrix nrs' ncs' rs) =
      maybe True (==nrs') nrs
    && maybe True (==ncs') ncs
    && V.all (V.all isZero) rs
  m == m'@(Zero _ _) = m' == m

  (One nrs a) == (One nrs' a') = 
       ( fromMaybe True $ (==) <$> nrs <*> nrs' )
    && a == a'
  (One nrs a) == (VVMatrix nrs' ncs' rs') =
       maybe True (==nrs') nrs
    && maybe True (==ncs') nrs
    && (`iall` rs') ( \ix -> iall
                    ( \jx a -> if jx/=ix then isZero a else isOne a ) )
    where
    iall :: (Int -> a -> Bool) -> Vector a -> Bool
    iall f = V.ifoldr' (\ix a b -> b && f ix a) True
  m == m'@(One _ _) = m' == m


instance ( AdditiveMonoid a, Show a ) => Show (VVMatrix a) where
  show m@(Zero nrs ncs) =
    ( \str -> maybe str show $ forceVVMay m )
      ("VVMatrix.Zero " ++ show nrs ++ " " ++ show ncs )
  show m@(One nrs a)    =
    ( \str -> maybe str  show $ forceVVMay m )
      ("VVMatrix.One " ++ show nrs ++ " " ++ show a)
  show (VVMatrix 0 _ rs) = "[ ]"
  show (VVMatrix _ 0 rs) = "[ ]"
  show (VVMatrix _ _ rs) =
    V.foldl1 (\r r' -> r ++ "\n" ++ r') $ V.map show' rsShown
    where
    rsShown = V.map (V.map show) rs
    show' r= "[ " ++ rShown ++ " ]"
      where
      rShown = V.foldl1 (\a a' -> a ++ " " ++ a') $ V.map center r
    center s = replicate n ' ' ++ s ++ replicate n' ' '
      where
      maxLength = V.maximum $ V.map (V.maximum . V.map length) rsShown
      n = (maxLength - length s) `div` 2
      n' = maxLength - n - length s
