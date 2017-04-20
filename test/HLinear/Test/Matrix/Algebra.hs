{-# LANGUAGE
    DataKinds
  #-}

module HLinear.Test.Matrix.Algebra
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy
import Math.Structure.Tasty

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Vector

import HLinear.Matrix
import HLinear.Matrix.Sized

import HLinear.Test.Utils


properties :: TestTree
properties =
  testGroup "Matrix" $
    ( (`runTestR` QC.testProperty ) $
      fmap concat $ sequence
      [ isRing ( Proxy :: Proxy (MatrixSized 1 1 Rational) )
      , isRing ( Proxy :: Proxy (MatrixSized 2 2 Rational) )
      , isRing ( Proxy :: Proxy (MatrixSized 3 3 Rational) )
      , isRing ( Proxy :: Proxy (MatrixSized 9 9 Rational) )
      ]
    ) 