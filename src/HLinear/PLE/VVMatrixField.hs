{-# LANGUAGE
    TypeFamilies
  #-}

-- there is an implementation for the lu decomposition in the matrix package,
-- but it seems hard to parallelize and also is only available for Rational,
-- which does not contain all fields.
module HLinear.PLE.VVMatrixField
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import qualified Data.Vector as V

import Data.Vector ( Vector(..), (!) )
import Control.Arrow ( first
                     , (***)
                     )
-- import Data.Default
import qualified Data.Permute as P
import Data.Permute ( permute
                    , swaps
                    , swapsPermute
                    )
import Math.Structure

import HLinear.PLE.PLE
import qualified HLinear.PLE.ReversePermute as RP
import HLinear.PLE.ReversePermute ( ReversePermute(..)
                                  , swapReversePermute
                                  )
import qualified HLinear.PLE.VVMatrixField.EchelonForm as EF
import HLinear.PLE.VVMatrixField.EchelonForm ( EchelonForm(..) )
import qualified HLinear.PLE.VVMatrixField.LeftTransformation as LT
import HLinear.PLE.VVMatrixField.LeftTransformation ( LeftTransformation(..) )
import HLinear.VVMatrix.Definition ( VVMatrix(..) )


instance (DecidableZero a, Field a) => HasPLE (VVMatrix a) where
  type PLELeft (VVMatrix a) = LeftTransformation a
  type PLEEchelon (VVMatrix a) = EchelonForm a

  -- todo: later use  runReader def $ pleSlice m
  ple = pleColumn 

  left' = LT.toVVMatrix
  echelon' = EF.toVVMatrix


pleColumn :: (DecidableZero a, Field a) 
          => VVMatrix a
          -> PLEDecomposition (VVMatrix a)
pleColumn m@(VVMatrix nrs ncs rs)
  | nrs == 0 || ncs == 0
    = PLEDecomposition $ PLEHook
      (RP.reversePermute nrs)
      (LeftTransformation nrs 0 V.empty)
      (EchelonForm 0 ncs V.empty)
  | otherwise =
  let
  pIx' = V.findIndex ((not . isZero) . V.head) rs
  mTail = VVMatrix nrs (ncs-1) $ V.map V.tail rs
  in
  case pIx' of
    Nothing  -> shiftPLEDecomposition 1 $ pleColumn mTail
    Just pIx -> mergeHook hook $ pleColumn (reduceByHook hook mTail)
      where
      pivotRecip = recip $ nonZero $ V.head (rs ! pIx)
      negCol = V.map negate $
               V.generate (nrs-1) $ \i ->
                 V.head $ rs ! (if i < pIx then i else i+1)

      hook = PLEHook p l e
      p = swapReversePermute nrs (0,pIx)
      l = LeftTransformation nrs 1 $ V.singleton (pivotRecip, negCol)
      e = EchelonForm 1 ncs $ V.singleton (0, erow)
        where
        erow = one `V.cons` V.map (*(fromNonZero pivotRecip)) (V.tail $ V.head rs)

 -- | shift PLE decomposition by a given number of columns
shiftPLEDecomposition :: Int -> PLEDecomposition (VVMatrix a)
                      -> PLEDecomposition (VVMatrix a)
shiftPLEDecomposition s (PLEDecomposition (PLEHook p l e)) =
  PLEDecomposition $ PLEHook p l $ EF.shift s e

mergeHook :: (Field a)
          => PLEHook (VVMatrix a) -> PLEDecomposition (VVMatrix a)
          -> PLEDecomposition (VVMatrix a)
mergeHook (PLEHook p l e)
          (PLEDecomposition (PLEHook p' l' e')) =
  PLEDecomposition $ PLEHook p'' l'' e''
  where
  p'' = RP.concat p p'
  l'' = LT.concat l l'
  e'' = EF.concat e e'

reduceByHook :: (Field a)
             => PLEHook (VVMatrix a) -> VVMatrix a
             -> VVMatrix a
reduceByHook (PLEHook p l e) (VVMatrix nrs ncs rs) =
  LT.apply l $ VVMatrix nrs' ncs $
               V.slice (EF.nmbRows e) nrs' rsP
  where
  nrs' = nrs - EF.nmbRows e
  rsP = RP.apply p nrs rs 


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

