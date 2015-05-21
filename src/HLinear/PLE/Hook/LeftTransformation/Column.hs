{-# LANGUAGE
    FlexibleContexts
  , MultiParamTypeClasses
  #-}

module HLinear.PLE.Hook.LeftTransformation.Column
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Applicative ( (<$>) )
import Control.Arrow ( first )
import Data.Maybe
import Data.Vector ( Vector(..) )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ( Natural )


import HLinear.PLE.Hook.ReversePermute
import HLinear.VVMatrix hiding ( (!), (!?) )
import HLinear.VVMatrix.Utils
import HLinear.VVMatrix.Definition ( VVMatrix(..) )


data LeftTransformationColumn a =
  LeftTransformationColumn Int (NonZero a) (Vector a)
  deriving Show

(!) :: LeftTransformationColumn a -> Int -> a
(!) (LeftTransformationColumn offset a vs) ix
  | ix < offset  = error "LeftTransformationColumn (!) out of range"
  | ix == offset = fromNonZero a
  | otherwise    = vs V.! (ix - offset - 1)


ltcShiftOffset :: Int -> LeftTransformationColumn a
              -> LeftTransformationColumn a
ltcShiftOffset shift (LeftTransformationColumn offset a vs) =
  LeftTransformationColumn (offset + shift) a vs

ltcHead :: LeftTransformationColumn a -> a  
ltcHead (LeftTransformationColumn _ a _) = fromNonZero a

ltcHeadNonZero :: LeftTransformationColumn a -> NonZero a
ltcHeadNonZero (LeftTransformationColumn _ a _) = a

ltcHeadRecip :: MultiplicativeGroup (NonZero a)
            => LeftTransformationColumn a -> a  
ltcHeadRecip (LeftTransformationColumn _ na _) = fromNonZero $ recip na

ltcTail :: LeftTransformationColumn a -> Vector a
ltcTail (LeftTransformationColumn _ _ v) = v
