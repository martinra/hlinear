{-# LANGUAGE
    StandaloneDeriving
  #-}

module HLinear.PLE.Hook.EchelonForm.Basic
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import Control.Arrow ( first )
import qualified Data.Vector as V
import Data.Vector ( Vector(..) )
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.PLE.Hook.EchelonForm.Definition as EF
import HLinear.PLE.Hook.EchelonForm.Row as EFR

import Debug.Trace


-- properties

offset :: EchelonForm a -> Natural
offset (EchelonForm nrs _ rs)
  | V.null rs = nrs
  | otherwise = EFR.offset $ V.head rs

-- Eq and Show

deriving instance Show a => Show (EchelonForm a)

instance ( Eq a, DecidableZero a ) => Eq (EchelonForm a) where
  (EchelonForm nrs ncs rs) == (EchelonForm nrs' ncs' rs') =
    nrs == nrs' && ncs == ncs' && rs == rs'

-- conversion

toMatrix :: AdditiveMonoid a => EchelonForm a -> Matrix a
toMatrix (EchelonForm nrs ncs rs) = Matrix nrs ncs rs'
  where
    hrs = V.map EFR.row rs
    rs' = V.map EFR.toVector rs V.++ zeros
    zeros = V.replicate ((fromIntegral nrs) - V.length rs)
                        (V.replicate (fromIntegral ncs) zero)

-- creation

singletonLeadingOne :: MultiplicativeMonoid a
                     => Natural -> Natural -> Vector a
                     -> EchelonForm a
singletonLeadingOne nrs o v = singleton nrs o $ one `V.cons` v

singleton :: Natural -> Natural -> Vector a
           -> EchelonForm a
singleton nrs o v = EchelonForm nrs nv $
                      V.singleton $ EchelonFormRow o v
  where
    nv = fromIntegral $ V.length v

-- submatrices

splitAt :: Int -> EchelonForm a -> (EchelonForm a, EchelonForm a)
splitAt ix (EchelonForm nrs ncs rs) =
  ( EchelonForm nrs ncs rsTop
  , EchelonForm nrs' ncs rsBottom
  )
  where
    nrsZ = fromIntegral nrs
    ixB = min nrsZ $ max 0 ix
    (rsTop,rsBottom) = V.splitAt ix rs
    nrs' = fromIntegral $ nrsZ - ixB
