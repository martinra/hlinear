{-# LANGUAGE
    DataKinds
  #-}

module HLinear.Test.Matrix.Algebra
where

import HLinear.Utility.Prelude

import Data.Ratio ( Rational )

import HLinear.Matrix.Sized
import Math.Structure.Tasty
import Test.Tasty


properties :: TestTree
properties =
  testGroup "Algebraic properties" $
    runTestsQC
    [ isAdditiveSemigroup ( Proxy :: Proxy (MatrixSized 3 5 Rational) )
    , isRing ( Proxy :: Proxy (MatrixSized 1 1 Rational) )
    , isRing ( Proxy :: Proxy (MatrixSized 2 2 Rational) )
    , isRing ( Proxy :: Proxy (MatrixSized 3 3 Rational) )
    , isRing ( Proxy :: Proxy (MatrixSized 9 9 Rational) )
    , isMultiplicativeLeftAction
        ( Proxy :: Proxy ( MatrixSized 3 3 Rational ) )
        ( Proxy :: Proxy ( MatrixSized 3 5 Rational ) )
    ]
