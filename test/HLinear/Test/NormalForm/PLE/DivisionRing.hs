{-# LANGUAGE
    ScopedTypeVariables
  #-}

module HLinear.Test.NormalForm.PLE.DivisionRing
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy
import HFlint.FMPZ
import HFlint.NMod
import Math.Structure
import Math.Structure.Tasty
import Test.Tasty

import HLinear.Matrix as M

import HLinear.Hook.PLEHook ( PLEHook(..) )
import HLinear.NormalForm.PLE ( ple )


properties :: TestTree
properties =
  testGroup "PLE Properties"
  [ testPropertyQSnC 2 "recombine ple decomposition (small prime)" $
      recombinePLE 3
      
  , testPropertyQSnC 2 "recombine ple decomposition (large prime)" $
      recombinePLE 1125899906842679
  ]

recombinePLE :: FlintLimb -> Matrix FMPZ -> Bool
recombinePLE p m =
  withNModContext p $ \(_ :: Proxy ctx) ->
    let mNMod = fmap toNMod m :: Matrix (NMod ctx)
        PLEHook p l e = ple mNMod
    in  mNMod == toMatrix p * toMatrix l * toMatrix e
