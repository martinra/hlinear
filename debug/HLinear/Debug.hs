{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  #-}

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
import Data.Bits
import Data.Maybe
import Data.Proxy
import Data.Ratio ( (%) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import HFlint.FMPQ
import HFlint.FMPZ
import Math.Structure
import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck as SC
import qualified Test.SmallCheck.Series as SCS


import HLinear.PLE
import HLinear.PLE.Decomposition as Decomp
import HLinear.PLE.FoldUnfold.Echelonize as Echelonize
import HLinear.PLE.FoldUnfold.ReducedEchelonForm as REF
import HLinear.PLE.Sliced.Echelonize
import HLinear.PLE.Strategy.NMod
import HLinear.PLE.Hook as Hook
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm.Row as EFR
import HLinear.PLE.Hook.EchelonTransformation as ET
import HLinear.PLE.Hook.EchelonTransformation.Column as ETC
import HLinear.PLE.Hook.RPermute as RP
import qualified HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.Matrix as M

import HLinear.Matrix.MultiMod
import HLinear.MultiMod.Definition as MM
import HLinear.MultiMod.Reconstruction as MM

import HFlint.NMod
import HFlint.Primes

import HLinear.Bench.Random

import Debug.Trace


main :: IO ()
main = do
  let Right m = M.fromLists' 5 2
                  [ [  0,  -1  ]
                  , [  0,   1  ]
                  , [  0,  12  ]
                  , [  0,  12  ]
                  , [ -3,  -11 ]
                  ]

  let p = 3
  let param = PLEDecompositionSlicedParameters
                SlicingBalanced (SlicingNmb 5)

--  let (pm,lm,em, om,rm) = withNModContext p $ \(_ :: Proxy ctx) ->
--        let mNMod = fmap toNMod (m :: Matrix FMPZ) :: Matrix (NMod ctx)
--            (pm',lm',em') = Decomp.toMatrices $
--                              pleDecompositionSliced param PLEStrategyNModFoldUnfold mNMod
--        in  ( fmap unNMod pm'
--            , fmap unNMod lm'
--            , fmap unNMod em'
--            , fmap unNMod mNMod
--            , fmap unNMod $ pm' * lm' * em'
--            )
  let (pm,lm,em, om,rm) = withNModContext p $ \(_ :: Proxy ctx) ->
        let mNMod = fmap toNMod (m :: Matrix FMPZ) :: Matrix (NMod ctx)
            (pm',lm',em') = Decomp.toMatrices $
                              pleDecompositionSliced param PLEStrategyNModFoldUnfold mNMod
        in  ( fmap unNMod pm'
            , fmap unNMod lm'
            , fmap unNMod em'
            , fmap unNMod mNMod
            , fmap unNMod $ pm' * lm' * em'
            )
  
  print om
  putStrLn "NNN"
  print rm
  putStrLn "NNN"

  print pm
  putStrLn "NNN"
  print lm
  putStrLn "NNN"
  print em

