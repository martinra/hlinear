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
  LeftTransformation Natural (Vector (LeftTransformationColumn a))
  deriving Show

nmbRows :: LeftTransformation a -> Natural
nmbRows (LeftTransformation nrs _) = nrs

nmbCols :: LeftTransformation a -> Natural
nmbCols (LeftTransformation _ cs) = fromIntegral $ V.length cs

ltDrop :: Int -> LeftTransformation a -> LeftTransformation a
ltDrop ix (LeftTransformation nrs cs) = LeftTransformation nrs' $ V.drop ix cs
  where
  nrs' = fromIntegral $ fromIntegral nrs - ix

instance Eq a => Eq (LeftTransformation a) where
  (LeftTransformation nrs cs) == (LeftTransformation nrs' cs') =
    nrs == nrs' && ncs == ncs'
    &&
    (`V.all` (V.zip cs cs')) (uncurry (==))
      where
      ncs = V.length cs
      ncs' = V.length cs'
