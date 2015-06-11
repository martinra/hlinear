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
import HLinear.BRMatrix ( nmbRows, nmbCols
                        , zeroMatrix
                        )
import HLinear.BRMatrix.Definition ( BRMatrix(..) )
import qualified HLinear.BRMatrix.RVector as RV


instance (DecidableZero a, DivisionRing a) => HasPLE (BRMatrix a) where
  type PLEPermute (BRMatrix a) = RPermute
  type PLELeft (BRMatrix a) = LeftTransformation a
  type PLEEchelon (BRMatrix a) = EchelonForm a

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
  fromPLEEchelon = EF.toMatrix


instance    (DecidableZero a, DivisionRing a)
         => MultiplicativeMagma (PLEHook (BRMatrix a)) where
  -- Note that the permutation and the left matrix are represented by their
  -- inverse. 
  (PLEHook p lt ef) * (PLEHook p' lt' ef') =
    PLEHook (p'*p) (lt' * (p' *. lt)) (ef * ef')

fromEchelonForm :: (DecidableZero a, DivisionRing a)
                => EchelonForm a -> PLEHook (BRMatrix a)
fromEchelonForm ef@(EchelonForm nrs _ _) = PLEHook
    (RP.rpermute $ fromIntegral nrs)
    (LeftTransformation nrs V.empty)
    ef

splitOffHook :: ( DecidableZero a, DivisionRing a )
             => BRMatrix a -> Maybe (PLEHook (BRMatrix a), BRMatrix a)
splitOffHook m@(BRMatrix nrs ncs rs)
  | nrs == 0 || ncs == 0 = Nothing
  | otherwise            = 
      case ($rs) $ RV.lift $
             V.findIndex ((not . isZero) . RV.lift V.head) of
        Nothing -> Just ( fromEchelonForm $
                          EchelonForm nrs ncs V.empty
                        , BRMatrix nrs (pred ncs) csTail
                        )
        Just pIx -> Just ( PLEHook p l e
                         , BRMatrix (pred nrs) (pred ncs) $
                           l *. (p *. (RV.liftRV V.tail csTail))
                         )
          where
          pivotRecip = recip $ nonZero $
                         RV.lift V.head (RV.lift (V.!pIx) rs)
          negCol = V.map negate $
            V.generate (fromIntegral nrs - 1) $ \ix ->
              RV.lift V.head $
              RV.lift (V.! if ix < pIx then ix else ix+1) rs
 
          p = RP.fromTransposition (fromIntegral nrs) (0,pIx)
          l = LeftTransformation nrs $ V.singleton $
              LT.LeftTransformationColumn 0 pivotRecip negCol
          e = EchelonForm 1 ncs $ V.singleton $
              EF.EchelonFormRow 0 $
              one `V.cons`
              V.map (fromNonZero pivotRecip *) (RV.lift V.tail $ RV.lift (V.!pIx) rs)
        where
          csTail = RV.liftRV (V.map $ RV.liftRV V.tail) rs




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

