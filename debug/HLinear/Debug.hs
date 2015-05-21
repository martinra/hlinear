module Main
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import qualified Data.Vector as V
import Math.Structure
import qualified Test.QuickCheck as QC

import HLinear.PLE.Hook.LeftTransformation


main = prod

prod :: IO ()
prod = do
  lt <- QC.generate $ QC.arbitrary `QC.suchThat` ((<3) . nmbRows) :: IO (LeftTransformation Rational)
  lt' <- QC.generate $ QC.arbitrary `QC.suchThat` ((<3) . nmbRows) :: IO (LeftTransformation Rational)

  let p = toMatrix $ lt * lt'
  let p' = toMatrix lt * toMatrix lt'

  print lt; print lt'
  print p; print p'; print (p - p')

inv :: IO ()
inv = do
  let lt = fromVector $ V.fromList [1 P./ 2, 2 P./ 3] :: LeftTransformation Rational
  let lti = recip lt
  print lt
  print lti
  print $ lt * lti
