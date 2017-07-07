{-# LANGUAGE OverloadedStrings #-}

module HLinear.Bench.MatrixParser
where

import HLinear.Utility.Prelude hiding ( many )

import Data.Attoparsec.ByteString.Char8
import Data.Ratio ( Rational, (%), numerator, denominator )
import Prelude ( fromRational )
import qualified Data.Vector as V

import HLinear.Matrix ( Matrix(..) )


fmpqParser :: Parser FMPQ
fmpqParser = do
  void $ string "MatrixFMPQ"
  void $ many1 space
  n <- decimal
  void $ many1 space
  d <- decimal 
  return $ fromRational (n % d)
  
matrixFMPQParser :: Parser (Matrix FMPQ)
matrixFMPQParser = do
  nrs <- decimal
  guard $ nrs >= 0
  void $ many1 space

  ncs <- decimal
  guard $ ncs >= 0
  void $ some space
  endOfLine

  rs <- V.replicateM ncs $ do
    r <- V.replicateM nrs (fmpqParser <* many' space)
    endOfLine
    return r

  return $ Matrix nrs ncs rs

showMatrixRational :: Matrix Rational -> String
showMatrixRational (Matrix nrs ncs rs) =
  V.foldl' (<>) ("MatrixFMPQ " <> show nrs <> " " <> show ncs <> "\n") $
  (\f -> fmap f rs) $ \r -> (<> "\n") $
    V.foldl' (<>) "" $
    (\f' -> fmap f' r) $ \e ->
      show (numerator e) <> " " <> show (denominator e) <> " "
