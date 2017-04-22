{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , TypeFamilies
  , TupleSections
  #-}

module HLinear.PLE.FoldUnfold.DivisionRing
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Arrow ( (&&&) )
import Data.Permute ( Permute )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural

import HLinear.PLE.Hook
import qualified HLinear.Utility.RPermute as RP
import HLinear.Utility.RPermute ( RPermute(..) )
import qualified HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm ( EchelonForm(..) )
import qualified HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.PLE.Hook.LeftTransformation ( LeftTransformation(..) )
import HLinear.Matrix ( Matrix(..), headRows, tailRows )
import qualified HLinear.Matrix as M



pleFoldUnfold
  :: ( DivisionRing a, DecidableZero a, DecidableUnit a, MultiplicativeGroup (Unit a) )
  => Matrix a -> PLEHook a
pleFoldUnfold m@(Matrix nrs ncs _) =
  V.foldl (*)
  ( PLEHook one (LT.one nrs) (EF.zero nrs ncs) )
  ( V.unfoldr splitOffHook m )


splitOffHook
  :: ( DivisionRing a, DecidableZero a, DecidableUnit a, MultiplicativeGroup (Unit a) )
  => Matrix a -> Maybe (PLEHook a, Matrix a)
splitOffHook m@(Matrix nrs ncs rs)
  | nrs == 0 || ncs == 0 = Nothing
  | otherwise            = Just $
      case V.findIndex ((not . isZero) . V.head) rs of
        Nothing  -> ( PLEHook one (LT.one nrs) $ EF.zero nrs ncs
                    , Matrix nrs (pred ncs) $ V.map V.tail rs
                    )
        Just pIx -> ( PLEHook p lt ef
                    , Matrix (pred nrs) (pred ncs) matRows
                    )
          where
          pivotRow = rs V.! pIx
          pivot = V.head pivotRow
          pivotRecip = recip $ toUnit pivot
          pivotTailRecip = V.map (fromUnit pivotRecip *) $ V.tail pivotRow

          bottomRows =
            if pIx == 0
            then V.tail rs
            else V.update (V.tail rs) $ V.singleton (pred pIx,V.head rs)
          (bottomHeads,bottomTails) =
            V.unzip $ V.map (V.head &&& V.tail) bottomRows
 
          p = RP.fromTransposition (fromIntegral nrs) (0,pIx)
          lt = LeftTransformation nrs $ V.singleton $
                 LT.LeftTransformationColumn 0
                   pivotRecip
                   ( V.map negate bottomHeads )
          ef = EF.singletonLeadingOne nrs pivotTailRecip
          
          matRows = V.zipWith
                      ( \h t -> V.zipWith (\pv te -> te - h * pv) pivotTailRecip t )
                      bottomHeads bottomTails
