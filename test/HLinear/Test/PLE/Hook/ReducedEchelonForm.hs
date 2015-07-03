module HLinear.Test.PLE.Hook.ReducedEchelonForm
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )


import Data.Maybe
import qualified Data.Vector as V
import Math.Structure

import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC

import HLinear.PLE.Hook
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonTransformation as ET
import HLinear.PLE.Hook.ReducedEchelonForm as REF
import HLinear.Matrix.Conversion
import qualified HLinear.Matrix as M

import HLinear.Test.Utils


reducedEchelonFormProperties :: TestTree
reducedEchelonFormProperties =
  testGroup "ReducedEchelonForm"
  [ testPropertyMatrix "recombine reduceLastPivot" $
      \ef -> let _ = ef :: EchelonForm Rational
             in fromMaybe True $ do
                  (EchelonReduction et m ef'', ef') <- reduceLastPivot ef
                  let efm = EF.toMatrix ef
                  let ef'm = EF.toMatrix ef'
                  let ef''m = EF.toMatrix ef''
                  let etm = ET.toMatrix et
                  let zm = M.zeroMatrix (EF.nmbRows ef'') (EF.nmbCols ef')
                  let im = M.identityMatrix (EF.nmbRows ef P.- ET.nmbRows et)
                  return $ M.blockSum etm im * efm == M.blockMatrixL [[ef'm,m],[zm,ef''m]]

  , testPropertyMatrix "recombine reducedEchelonForm" $
      \ef -> let (et,ef') = reduce (ef :: EchelonForm Rational)
                 efm = EF.toMatrix ef
                 ef'm = EF.toMatrix ef'
                 etm = ET.toMatrix et
                 im = M.identityMatrix (EF.nmbRows ef P.- ET.nmbRows et)
                 -- zm = M.zeroMatrix (fromIntegral $ EF.nmbRows ef P.- EF.nmbRows ef') (EF.nmbCols ef)
             in M.blockSum etm im * efm == ef'm
  ]
