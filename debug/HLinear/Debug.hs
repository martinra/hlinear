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
import HFlint.FMPQ
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

import HLinear.Matrix.MultiMod


main :: IO ()
main = do
  let Right m = M.fromLists [[1%2,3%4],[5%4,7%4]]
  let m' = fmap fromRational m :: Matrix FMPQ

  print m
  print m'
  putStrLn ""
  print $ m * m
  -- print $ mulMultiMod m' m'
