{-# LANGUAGE
    FlexibleContexts
  #-}

module HLinear.Test.Utils
where

-- copied from HFlint. This should be implemented in a separate package.

import Control.Applicative ( (<$>) )
import Control.Exception ( catch
                         , throw
                         , evaluate
                         , ArithException(..)
                         )
import Control.Monad ( liftM
                     , liftM2
                     )
import System.IO.Unsafe ( unsafePerformIO )

import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC


testProperty s p = testGroup "(QuickCheck & SmallCheck)"
  [ QC.testProperty s p
  , SC.testProperty s p
  ]

testPropertyVVMatrix :: (QC.Testable p, SC.Testable IO p)
                     => String -> p -> TestTree
testPropertyVVMatrix s p = testGroup "(QuickCheck & SmallCheck)"
  [ QC.testProperty s p
  , SC.testProperty s $ SC.changeDepth (const 0) p
  ] 


equal :: Eq a
      => (b -> c) -> (c -> b)
      -> (b -> a)
      -> (c -> a)
      -> b
      -> Bool
equal bToC _ f g x
  = f x == g (bToC x)

equal2 :: Eq a
       => (b -> c) -> (c -> b)
       -> (b -> b -> a)
       -> (c -> c -> a)
       -> b -> b
       -> Bool
equal2 bToC = equal2' bToC bToC

equal2' :: Eq a
       => (b -> c) -> (b' -> c') -> (c -> b)
       -> (b -> b' -> a)
       -> (c -> c' -> a)
       -> b -> b'
       -> Bool
equal2' bToC b'ToC' _ f g x y
  = f x y == g (bToC x) (b'ToC' y)

intertwining ::  Eq b 
             => (b -> c) -> (c -> b)
             -> (b -> b)
             -> (c -> c)
             -> b
             -> Bool
intertwining bToC cToB f g x
  = f x == cToB ( g (bToC x))

intertwining2 :: Eq b 
              => (b -> c) -> (c -> b)
              -> (b -> b -> b)
              -> (c -> c -> c)
              -> b -> b
              -> Bool
intertwining2 bToC cToB = intertwining2' bToC bToC cToB

intertwining2' :: Eq b 
               => (b -> c) -> (b' -> c') -> (c -> b)
               -> (b -> b' -> b)
               -> (c -> c' -> c)
               -> b -> b'
               -> Bool
intertwining2' bToC b'ToC' cToB f g x y
  = f x y == cToB ( g (bToC x) (b'ToC' y))


catchDivisionByZero :: IO (Maybe a) -> IO (Maybe a)
catchDivisionByZero a = a `catch` \e -> case e of
  DivideByZero         -> return Nothing
  RatioZeroDenominator -> return Nothing
  _                    -> throw e

wrapDivideByZero :: (a -> b)
                  -> Maybe a -> Maybe b
wrapDivideByZero f a = unsafePerformIO $
  catchDivisionByZero $
  case liftM f a of
    Nothing -> return Nothing
    Just c' -> Just <$> evaluate c'

wrapDivideByZero2 :: (a -> b -> c)
                  -> Maybe a -> Maybe b -> Maybe c
wrapDivideByZero2 f a b = unsafePerformIO $
  catchDivisionByZero $
  case liftM2 f a b of
    Nothing -> return Nothing
    Just c' -> Just <$> evaluate c'
