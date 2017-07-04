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
  testGroup "Algebraic properties"
    [ testGroup "Matrices of size 3 x 5" $ runTestsQC
      [ isAdditiveSemigroup ( Proxy :: Proxy (MatrixSized 3 5 Rational) ) ]
    , testGroup "Matrices of size 1 x 1" $ runTestsQC
      [ isRing ( Proxy :: Proxy (MatrixSized 1 1 Rational) ) ]
    , testGroup "Matrices of size 2 x 2" $ runTestsQC
      [ isRing ( Proxy :: Proxy (MatrixSized 2 2 Rational) ) ]
    , testGroup "Matrices of size 3 x 3" $ runTestsQC
      [ isRing ( Proxy :: Proxy (MatrixSized 3 3 Rational) ) ]
    , testGroup "Matrices of size 9 x 9" $ runTestsQC
      [ isRing ( Proxy :: Proxy (MatrixSized 9 9 Rational) ) ]
    , testGroup "Matrices of size 3 x 3 acting on 3 x 5" $ runTestsQC
      [ isMultiplicativeLeftAction
          ( Proxy :: Proxy ( MatrixSized 3 3 Rational ) )
          ( Proxy :: Proxy ( MatrixSized 3 5 Rational ) )
      ]
    ]
