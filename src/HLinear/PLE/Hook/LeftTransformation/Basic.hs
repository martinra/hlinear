{-# LANGUAGE
    StandaloneDeriving
  #-}

module HLinear.PLE.Hook.LeftTransformation.Basic
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


import HLinear.PLE.Hook.LeftTransformation.Column
import qualified HLinear.PLE.Hook.LeftTransformation.Column as LTC
import HLinear.PLE.Hook.RPermute

 -- \ A vector of columns (a, [v]) which are offset by their index.
 --   It represents a transformation from the left
 --     a1     0     0   0
 --   v*a1    a2     0   0
 --   v*a1  v*a2    a3   0 
 --   v*a1  v*a2  v*a3  a4
 --   . . . .
 --
data LeftTransformation a =
  LeftTransformation
    { nmbRows :: Natural
    , columns :: Vector (LeftTransformationColumn a)
    }

nmbCols :: LeftTransformation a -> Natural
nmbCols = fromIntegral . V.length . columns

-- Eq an Show instances

deriving instance Show a => Show (LeftTransformation a)

instance Eq a => Eq (LeftTransformation a) where
  (LeftTransformation nrs cs) == (LeftTransformation nrs' cs') =
    nrs == nrs' && ncs == ncs'
    &&
    (`V.all` (V.zip cs cs')) (uncurry (==))
      where
      ncs = V.length cs
      ncs' = V.length cs'

-- creation

identityLT :: Natural -> LeftTransformation a
identityLT nrs = LeftTransformation nrs V.empty

-- subtransformations

splitAt :: Int -> LeftTransformation a
        -> (LeftTransformation a, LeftTransformation a)
splitAt ix lt@(LeftTransformation nrs cs)
  | ix >= ncs = (lt, identityLT nrs')
  | otherwise =
      let (csLeft, csRight) = V.splitAt ix cs
      in ( LeftTransformation nrs csLeft
         , LeftTransformation nrs' $ V.map (LTC.setLength nrs'Z) csRight
         )
  where
    ncs = V.length cs
    nrs'Z = max 0 $ min nrsZ $ nrsZ - ix
    nrsZ = fromIntegral nrs
    nrs' = fromIntegral nrs'Z

drop :: Int -> LeftTransformation a -> LeftTransformation a
drop ix (LeftTransformation nrs cs) =
  LeftTransformation nrs' $ V.drop ix cs
  where
    nrs' = fromIntegral $ fromIntegral nrs - max 0 ix
