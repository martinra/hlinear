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
import HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.Matrix as M
import HLinear.PLE.Hook.PLE
import HLinear.PLE.Hook.RPermute as RP


main :: IO ()
main = do
  -- let m = M.Matrix 3 1 $ V.map V.singleton $ V.fromList [ (-3)%2, 4%7, 13%19 ] :: M.Matrix Rational 
  let m = M.Matrix 3 2 $ V.fromList $ map V.fromList
           [ [ (-1)%3, 7%4 ]
           , [ 1%19, 1%11]
           , [ 23%2, 31%2]
           ]
          :: Matrix Rational
                
--           [ [ (-8691316584051) % 6161142903677,  19957000698127 % 4750520093964,   8870450429191 % 2101117833362 ]
--           , [ 46906656993842 % 1163280750529,   9710292344567 % 3159527153430,    1850344055603 % 2120802527596 ]
--           , [ 1680697930226 % 2249983546553,   (-1287234071350) % 1225658150399,    447706484129 % 65985687573 ]
--           ]
--           [ (-145231158129) % 1965837109546
--           , 2721035879353 % 614656327869
--           , (-10496232632715) % 3989053149943
--           ]
  print m


  let Just (PLEHook p l e, m') = splitOffHook m
  let RPermute p' = p
  print p'
  print l
  print e
  print m'

  putStrLn ""; putStrLn ""

  let m'zero = if LT.nmbCols l == 1
              then M.zeroMatrix 1 1 `mappend` m'
              else M.zeroMatrix (M.nmbRows m) 1 `M.blockSumRows` m'
  let pm = RP.toMatrix p :: Matrix Rational
  let lm = LT.toMatrix l
  let em = EF.toMatrix e
  let d (Matrix nrs ncs _) = show [nrs,ncs]

  print "P"
  print pm
  print "L"
  print lm
  print "E"
  print em
  print "M'"
  print m'zero

  print "M"
  print m
  
  print "LM"
  print $ lm * m

  print $  lm * pm * m == m'zero + em


