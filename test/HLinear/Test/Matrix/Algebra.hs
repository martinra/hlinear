{-# LANGUAGE ScopedTypeVariables #-}

module HLinear.Test.Matrix.Algebra
where

import HLinear.Utility.Prelude

import HLinear.Matrix.Sized
import Math.Structure.Tasty
import Test.Tasty


properties
  :: forall ctx
  .  ReifiesNModContext ctx
  => Reader (Proxy ctx) TestTree
properties = pure $
  testGroup "Algebraic properties"
    [ testGroup "Matrices of size 3 x 5" $ runTestsQC
      [ isAdditiveSemigroup ( Proxy :: Proxy (MatrixSized 3 5 FMPZ) ) ]
    , testGroup "Matrices of size 1 x 1" $ runTestsQC
      [ isRing ( Proxy :: Proxy (MatrixSized 1 1 FMPQ) ) ]
    , testGroup "Matrices of size 2 x 2" $ runTestsQC
      [ isRing ( Proxy :: Proxy (MatrixSized 2 2 FMPQ) ) ]
    , testGroup "Matrices of size 3 x 3" $ runTestsQC
      [ isRing ( Proxy :: Proxy (MatrixSized 3 3 (NMod ctx)) ) ]
    , testGroup "Matrices of size 9 x 9" $ runTestsQC
      [ isRing ( Proxy :: Proxy (MatrixSized 9 9 (NMod ctx)) ) ]
    , testGroup "Matrices of size 3 x 3 acting on 3 x 5" $ runTestsQC
      [ isMultiplicativeLeftAction
          ( Proxy :: Proxy (MatrixSized 3 3 (NMod ctx)) )
          ( Proxy :: Proxy (MatrixSized 3 5 (NMod ctx)) )
      ]
    ]
