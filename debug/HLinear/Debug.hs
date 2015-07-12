module Main
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Ratio ( (%) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure
import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck as SC
import qualified Test.SmallCheck.Series as SCS

import HLinear.PLE.PLE
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm.Row as EFR
import HLinear.PLE.Hook.EchelonTransformation as ET
import HLinear.PLE.Hook.EchelonTransformation.Column as ETC
import HLinear.PLE.Hook.ReducedEchelonForm as REF
import HLinear.Matrix as M
import HLinear.PLE.Hook.PLE
import HLinear.PLE.Hook.RPermute as RP


main :: IO ()
main = do
  let ef = EchelonForm { EF.nmbRows = 1
                       , EF.nmbCols = 2
                       , EF.rows = V.fromList
                           [ EchelonFormRow { EFR.offset = 1
                                            , EFR.row = V.fromList [1%3]}
                           ]
                       }
            :: EchelonForm Rational

  let (et,ef') = reduce ef
  let efm = EF.toMatrix ef
  let ef'm = EF.toMatrix ef'
  let etm = ET.toMatrix et
  let im = M.identityMatrix (EF.nmbRows ef P.- ET.nmbRows et)
  -- let zm = M.zeroMatrix (fromIntegral $ EF.nmbRows ef P.- EF.nmbRows ef') (EF.nmbCols ef)

  print $ M.blockSum etm im * efm
  print "NN" 
  print ef'
  print $ ef'm

--  let firstReduction = EchelonReduction
--                         (ET.identityET $ EF.nmbRows ef)
--                         (M.zeroMatrix (EF.nmbRows ef) 0)
--                         (EF.zeroEF 0 0)
--                         :: EchelonReduction Rational
  print $ V.unfoldr reduceLastPivot ef
  let ef1 = EchelonForm 0 1 V.empty
  let m2  = Matrix 0 1 V.empty
  let ef2 = EchelonForm 1 1 $ V.singleton $ EchelonFormRow 0 $ V.singleton 1


  print ( EF.blockSumAtPivot ef1 m2 ef2 :: EchelonForm Rational )
--  print $ firstReduction * reds V.! 0

-- [ EchelonReduction
--     (EchelonTransformaion {nmbRows = 1, columns = fromList [EchelonTransformationColumn {offset = 0, init = fromList []}]})
--     [ Matrix 0 x 1 ]
--     (EchelonForm {nmbRows = 1, nmbCols = 1, rows = fromList [EchelonFormRow {offset = 0, row = fromList [1 % 1]}]})
-- ,  EchelonReduction
--     (EchelonTransformation {nmbRows = 0, columns = fromList []})
--     [ Matrix 0 x 1 ]
--     (EchelonForm {nmbRows = 0, nmbCols = 1, rows = fromList [EchelonFormRow {offset = 0, row = fromList [0 % 1]}]})]
-- 
