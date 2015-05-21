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
import qualified HLinear.PLE.Hook.ReversePermute as RP
import HLinear.PLE.Hook.ReversePermute ( ReversePermute(..) )
import qualified HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm ( EchelonForm(..) )
import qualified HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.PLE.Hook.LeftTransformation ( LeftTransformation(..) )
import HLinear.VVMatrix ( nmbRows, nmbCols
                        , zeroMatrix
                        )
import HLinear.VVMatrix.Definition ( VVMatrix(..) )
import HLinear.VVMatrix.MatrixRow


instance (DecidableZero a, DivisionRing a) => HasPLE (VVMatrix a) where
  type PLEPermute (VVMatrix a) = ReversePermute
  type PLELeft (VVMatrix a) = LeftTransformation a
  type PLEEchelon (VVMatrix a) = EchelonForm a

  -- todo: later use  runReader def $ pleSlice m
  ple m = PLEDecomposition $ V.foldl (*) firstHook $
          V.unfoldr splitOffHook m
    where
    firstHook = PLEHook
      (RP.reversePermute $ fromIntegral nrs)
      (LeftTransformation nrs V.empty)
      (EchelonForm nrs ncs V.empty)
    nrs = fromJust $ nmbRows m
    ncs = fromJust $ nmbCols m

  fromPLEPermute = VVMatrixPermute . RP.fromReversePermute . recip
  fromPLELeft = LT.toInverseMatrix
  fromPLEEchelon = EF.toMatrix


instance    (DecidableZero a, DivisionRing a)
         => MultiplicativeMagma (PLEHook (VVMatrix a)) where
  -- Note that the permutation and the left matrix are represented by their
  -- inverse. 
  (PLEHook p lt ef) * (PLEHook p' lt' ef') =
    PLEHook (p'*p) (lt' * (p' *. lt)) (ef * ef')

fromEchelonForm :: (DecidableZero a, DivisionRing a)
                => EchelonForm a -> PLEHook (VVMatrix a)
fromEchelonForm ef@(EchelonForm nrs _ _) = PLEHook
    (RP.reversePermute $ fromIntegral nrs)
    (LeftTransformation nrs V.empty)
    ef

splitOffHook :: ( DecidableZero a, DivisionRing a )
             => VVMatrix a -> Maybe (PLEHook (VVMatrix a), VVMatrix a)
splitOffHook (Zero (Just nrs) (Just ncs))
  | nrs == 0 || ncs == 0 = Nothing
  | otherwise            = Just $ (,zeroMatrix 0 0) $ fromEchelonForm $
      EchelonForm nrs ncs V.empty
splitOffHook (One (Just nrs) a)
  | nrs == 0  = Nothing
  | otherwise = Just $ (,zeroMatrix 0 0) $ fromEchelonForm $
      EchelonForm nrs nrs $ V.generate (fromIntegral nrs) $
        \ix -> EF.EchelonFormRow (fromIntegral ix) $
                 a `V.cons` V.replicate (fromIntegral nrs - ix - 1) zero
splitOffHook m@(VVMatrix nrs ncs rs)
  | nrs == 0 || ncs == 0 = Nothing
  | otherwise            =
      case V.findIndex ((not . isZero) . V.head) rs of
        Nothing -> Just ( fromEchelonForm $
                          EchelonForm nrs ncs V.empty
                        , VVMatrix nrs (pred ncs) csTail
                        )
        Just pIx -> Just ( PLEHook p l e
                         , VVMatrix (pred nrs) (pred ncs) $
                           V.map unMatrixRow $
                           l *. (p *. V.map MatrixRow (V.tail csTail))
                         )
          where
          pivotRecip = recip $ nonZero $ V.head (rs V.! pIx)
          negCol = V.map negate $
            V.generate (fromIntegral nrs - 1) $ \ix ->
              V.head $ rs V.! (if ix < pIx then ix else ix+1)
 
          p = RP.fromTransposition (fromIntegral nrs) (0,pIx)
          l = LeftTransformation nrs $ V.singleton $
              LT.LeftTransformationColumn 0 pivotRecip negCol
          e = EchelonForm 1 ncs $ V.singleton $
              EF.EchelonFormRow 0 $
              one `V.cons`
              V.map (fromNonZero pivotRecip *) (V.tail $ rs V.! pIx)
        where
          csTail = V.map V.tail rs




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

