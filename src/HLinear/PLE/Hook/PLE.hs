{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , TypeFamilies
  , TupleSections
  #-}

-- there is an implementation for the lu decomposition in the matrix package,
-- but it seems hard to parallelize and also is only available for Rational,
-- which does not contain all fields.
module HLinear.PLE.Hook.PLE
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import qualified Data.Vector as V

import Data.Maybe
import Data.Vector ( Vector(..) )
import Control.Arrow ( first
                     , (***)
                     )
import Control.Monad.Identity
-- import Data.Default
import qualified Data.Permute as P
import Data.Permute ( permute
                    , swaps
                    , swapsPermute
                    )
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.PLE.PLE
import qualified HLinear.PLE.Hook.RPermute as RP
import HLinear.PLE.Hook.RPermute ( RPermute(..) )
import qualified HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm ( EchelonForm(..) )
import qualified HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.PLE.Hook.LeftTransformation ( LeftTransformation(..) )
import qualified HLinear.BRMatrix as BRM
import HLinear.Matrix ( nmbRows, nmbCols
                      , zeroMatrix
                      , headRows, tailRows
                      )
import HLinear.Matrix.Conversion
import HLinear.Matrix.Definition ( Matrix(..) )

-- instance (DecidableZero a, DivisionRing a) => HasPLE (BRMatrix a) where
--   type PLEPermute (BRMatrix a) = RPermute
--   type PLELeft (BRMatrix a) = LeftTransformation a
--   type PLEEchelon (BRMatrix a) = EchelonForm a

instance (DecidableZero a, DivisionRing a) => HasPLE (Matrix a) where
  type PLEPermute (Matrix a) = RPermute
  type PLELeft (Matrix a) = LeftTransformation a
  type PLEEchelon (Matrix a) = EchelonForm a

  -- todo: later use  runReader def $ pleSlice m
  ple m = PLEDecomposition $ V.foldl (*) firstHook $
          V.unfoldr splitOffHook m
    where
    firstHook = PLEHook
      (RP.rpermute $ fromIntegral nrs)
      (LeftTransformation nrs V.empty)
      (EchelonForm nrs ncs V.empty)
    nrs = nmbRows m
    ncs = nmbCols m

  fromPLEPermute = MatrixPermute . RP.toPermute . recip
  fromPLELeft = LT.toInverseMatrix
  fromPLEEchelon =  EF.toMatrix


instance    (DecidableZero a, DivisionRing a)
         => MultiplicativeMagma (PLEHook (Matrix a)) where
  -- This is partially defined. The right factor must be smaller than the
  -- largest contribution of the LeftTransformation of the left factor.
  -- Note that the permutation and the left matrix are represented by their
  -- inverse. 
  (PLEHook p lt ef) * (PLEHook p' lt' ef') =
    PLEHook (p'*p) (lt' * (p' *. lt)) (ef * ef')

fromEchelonForm :: (DecidableZero a, DivisionRing a)
                => EchelonForm a -> PLEHook (Matrix a)
fromEchelonForm ef@(EchelonForm nrs _ _) = PLEHook
    (RP.rpermute $ fromIntegral nrs)
    (LeftTransformation nrs V.empty)
    ef

splitOffHook :: ( DecidableZero a, DivisionRing a )
             => Matrix a -> Maybe (PLEHook (Matrix a), Matrix a)
splitOffHook m@(Matrix nrs ncs rs)
  | nrs == 0 || ncs == 0 = Nothing
  | otherwise            = Just $
      case V.findIndex ((not . isZero) . V.head) rs of
        Nothing -> ( fromEchelonForm $ EchelonForm nrs ncs V.empty
                   , Matrix nrs (pred ncs) rsTails
                   )
        Just pIx -> pleFromPLM p l $ Matrix nrs (pred ncs) rsTails
          where
          pivotRecip = recip $ nonZero $ V.head (rs V.! pIx)
          negCol = V.map negate $
            V.generate (fromIntegral nrs - 1) $ \ix ->
              V.head $ (V.! if ix < pIx then ix else ix+1) rs
 
          p = RP.fromTransposition (fromIntegral nrs) (0,pIx)
          l = LeftTransformation nrs $ V.singleton $
              LT.LeftTransformationColumn 0 pivotRecip negCol
          
      where
        rsTails = V.map V.tail rs

pleFromPLM :: ( DivisionRing a, DecidableZero a )
           => RPermute -> LeftTransformation a -> Matrix a
           -> ( PLEHook (Matrix a), Matrix a )
pleFromPLM p l m = ( PLEHook p l e, tailRows plm )
  where
    plm = fromPLMatrix $ l *. (p *. PLMatrix m)
    e = EF.fromVector $ headRows plm

newtype PLMatrix a = PLMatrix {fromPLMatrix :: Matrix a}

instance MultiplicativeSemigroupLeftAction RPermute (PLMatrix a) where
  p *. PLMatrix (Matrix nrs ncs rs) = PLMatrix $
    Matrix nrs ncs $ RP.fromRPVector $ p *. RP.RPVector rs

instance    ( DivisionRing a, DecidableZero a )
         => MultiplicativeSemigroupLeftAction
              (LeftTransformation a) (PLMatrix a)
  where
  lt *. PLMatrix m@(Matrix nrs ncs _) = PLMatrix $ 
    fromBRMatrixUnsafe nrs ncs $ lt *. toBRMatrix m
    

-- type PLEParametrizedFunction a =
--   (Field a) 
--   => VVMatrix a
--   -> Reader PLEParameters (PLEDecomposition (VVMatrix a))
-- 
-- data PLEParameters m = PLEParameters
--   { cutOff :: Int
--   , pleBaseCase :: PLEParametrizedFunction
--   }
-- 
-- instance Default PLEParameters m where
--   def = PLEParameters
--     { cutOff = 20 
--     , pleBaseCase = pleColumn
--     }
-- 
-- pleSlice :: PLEParametrizedFunction
-- pleSlice m@(VVMatrix nrs ncs rs) = 
--   let
--   in do
--   _cutOff <- asks cutOff
--   if ncs > _cutOff
--     then return $ mergeHook hook (ple blockRight)
--          where
--          (hook, blockRight) = toHook (ple sliceLeft) sliceRight
--          (sliceLeft, sliceRight) = cutSlice m
--     else asks pleBaseCase >>= ($m)

