{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , TypeFamilies
  , TupleSections
  #-}

module HLinear.PLE.FoldUnfold.Echelonize
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Permute ( Permute )
import qualified Data.Vector as V
import Math.Structure

import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.Hook
import HLinear.PLE.Hook.PLMatrix
import qualified HLinear.PLE.Hook.RPermute as RP
import HLinear.PLE.Hook.RPermute ( RPermute(..) )
import qualified HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm ( EchelonForm(..) )
import qualified HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.PLE.Hook.LeftTransformation ( LeftTransformation(..) )
import qualified HLinear.BRMatrix as BRM
import HLinear.Matrix ( Matrix(..), headRows, tailRows )
import qualified HLinear.Matrix as M
import HLinear.Matrix.Conversion


class HasPLEDecompositionFoldUnfold f a where
  pleDecompositionFoldUnfold :: f a -> PLEDecomposition a


instance
     ( DecidableZero a, DivisionRing a )
  => HasPLEDecompositionFoldUnfold Matrix a
  where
  pleDecompositionFoldUnfold m =
    PLEDecomposition $ V.foldl (*) firstHook $ V.unfoldr splitOffHook m
    where
    nrs = M.nmbRows m
    ncs = M.nmbCols m

    firstHook = PLEHook
      (RP.rpermute $ fromIntegral nrs)
      (LeftTransformation nrs V.empty)
      (EchelonForm nrs ncs V.empty)



splitOffHook
  :: ( DecidableZero a, DivisionRing a )
  => Matrix a -> Maybe (PLEHook a, Matrix a)
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

        fromEchelonForm ef@(EchelonForm nrs _ _) = PLEHook
            (RP.rpermute $ fromIntegral nrs)
            (LeftTransformation nrs V.empty)
            ef

        pleFromPLM p l m = ( PLEHook p l e, tailRows plm )
          where
            plm = fromPLMatrix $ l *. (p *. PLMatrix m)
            e = EF.singletonLeadingOne (M.nmbRows m) 0 $ headRows plm
