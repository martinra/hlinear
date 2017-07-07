{-# LANGUAGE OverloadedStrings #-}

module HLinear.Bench.MatrixParser
where

import HLinear.Utility.Prelude

import Data.Attoparsec.ByteString.Char8
import Data.Ratio ( Rational, (%), numerator, denominator )
import Prelude ( fromRational )
import qualified Data.Vector as V

import HLinear.Matrix ( Matrix(..) )


parserFMPQ :: Parser FMPQ
parserFMPQ = do
  n <- signed decimal
  void $ many1 space
  d <- decimal 
  return $ fromRational (n % d)
  
parserMatrixFMPQ :: Parser (Matrix FMPQ)
parserMatrixFMPQ = do
  void $ string "MatrixFMPQ" <* many1 space

  nrs <- decimal
  guard $ nrs >= 0
  void $ many1 space

  ncs <- decimal
  guard $ ncs >= 0
  void $ many1 space

  rs <- V.replicateM nrs $
    V.replicateM ncs (parserFMPQ <* many1 space)

  return $ Matrix nrs ncs rs

showMatrixRational :: Matrix Rational -> String
showMatrixRational (Matrix nrs ncs rs) =
  V.foldl' (<>) ("MatrixFMPQ " <> show nrs <> " " <> show ncs <> "\n") $
  (\f -> fmap f rs) $ \r -> (<> "\n") $
    V.foldl' (<>) "" $
    (\f' -> fmap f' r) $ \e ->
      show (numerator e) <> " " <> show (denominator e) <> " "
