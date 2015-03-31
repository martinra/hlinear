{-# LANGUAGE
    DataKinds
  , KindSignatures
  #-}

module HLinear.VVMatrix.Basic
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Applicative ( (<$>), (<*>) )
import Data.Bool ( bool )
import Data.Composition ( (.:) )
import Data.Maybe
import Data.Proxy ( Proxy )
import qualified Data.Vector as V
import Data.Vector ( Vector )
import GHC.TypeLits ( Nat, KnownNat, natVal )
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.VVMatrix.Creation
import HLinear.VVMatrix.Definition ( VVMatrix(..), SizedVVMatrix(..) )
import HLinear.VVMatrix.Utils


toSized :: ( KnownNat nrs, KnownNat ncs )
        => Proxy (nrs :: Nat) -> Proxy (ncs :: Nat) -> VVMatrix a
        -> Maybe (SizedVVMatrix mrs ncs a)
toSized pnrs pncs (Zero nrs' ncs') =
  cmbDimMay (fromInteger $ natVal pnrs) nrs' >>= \nrs ->
  cmbDimMay (fromInteger $ natVal pncs) ncs' >>= \ncs ->
  Just ( SizedVVMatrix $ Zero (Just nrs) (Just ncs) )
toSized pnrs pncs (One nrs' a) =
  cmbDimMay (fromInteger $ natVal pnrs) nrs' >>=
  cmbDim    (fromInteger $ natVal pncs)      >>= \nrs ->
  Just ( SizedVVMatrix $ One (Just nrs) a )
toSized pnrs pncs m@(VVMatrix nrs' ncs' _) =
  cmbDim (fromInteger $ natVal pnrs) nrs'    >>
  cmbDim (fromInteger $ natVal pncs) ncs'    >>
  Just ( SizedVVMatrix m )


nmbRows :: VVMatrix a -> Maybe Natural
nmbRows (Zero nrs _) = nrs
nmbRows (One nrs _)  = nrs
nmbRows (VVMatrix nrs _ _) = Just nrs

nmbCols :: VVMatrix a -> Maybe Natural
nmbCols (Zero _ ncs) = ncs
nmbCols (One ncs _)  = ncs
nmbCols (VVMatrix _ ncs _) = Just ncs


(!) :: ( AdditiveMonoid a, MultiplicativeMonoid a )
    => VVMatrix a -> Int -> Vector a
(!) = fromJust .: (!?)

(!?) :: ( AdditiveMonoid a, MultiplicativeMonoid a )
    => VVMatrix a -> Int -> Maybe (Vector a)
(Zero nrs ncs) !? ix = (ix<) <$> fromIntegral <$> nrs >>=
                       bool Nothing (Just True) >>
                       (`V.replicate` zero) <$> fromIntegral <$> ncs
(One nrs a) !? ix =
    (ix<) <$> fromIntegral <$> nrs >>= bool Nothing (Just True) >>
    ( `V.generate` (bool zero a . (==ix)) ) <$> fromIntegral <$> nrs
(VVMatrix _ _ rs) !? ix = rs V.!? ix


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
      ( fromMaybe True $
        (&&) <$> ((==) <$> nrs <*> nrs')
             <*> ((==) <$> ncs <*> ncs') )
  (Zero nrs ncs) == (One nrs' a') =
         ( fromMaybe True $
           (&&) <$> ((==) <$> nrs <*> nrs')
                <*> ((==) <$> ncs <*> nrs') )
      && (isZero a')
  (One nrs a) == (Zero nrs' ncs') =
         ( fromMaybe True $
           (&&) <$> ((==) <$> nrs <*> nrs')
                <*> ((==) <$> nrs <*> ncs') )
      && (isZero a)
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
                      ( \jx a' -> if jx/=ix then isZero a' else a==a' ) )
      where
      iall :: (Int -> a -> Bool) -> Vector a -> Bool
      iall f = V.ifoldr' (\ix a b -> b && f ix a) True
  m == m'@(One _ _) = m' == m


instance ( AdditiveMonoid a, Show a ) => Show (VVMatrix a) where
  show m@(Zero nrs ncs) =
    "VVMatrix.Zero (" ++ show nrs ++ ") (" ++ show ncs ++ ")"
  show m@(One nrs a)    =
    "VVMatrix.One (" ++ show nrs ++ ") (" ++ show a ++ ")"
  show (VVMatrix 0 ncs rs) = "VVMatrix.Zero' 0 " ++ show ncs
  show (VVMatrix nrs 0 rs) = "VVMatrix.Zero' " ++ show nrs ++ " 0"
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



instance   ( DecidableZero a, DecidableOne a, Eq a)
         =>  Eq (SizedVVMatrix nrs ncs a) where
  (SizedVVMatrix m) == (SizedVVMatrix m') = m == m'

instance ( AdditiveMonoid a, Show a ) => Show (SizedVVMatrix nrs ncs a) where
  show (SizedVVMatrix m) = show m
