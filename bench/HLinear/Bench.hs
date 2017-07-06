{-# LANGUAGE
    FlexibleInstances
  , ScopedTypeVariables
  #-}

module Main
where

import HLinear.Utility.Prelude

import Data.Attoparsec ( parseOnly )
import Data.ByteString ( readFile )
import Criterion.Main
import Criterion.Types ( Config(..), Verbosity(..) )
import Options.Applicative
import System.FilePath ( FilePath )
import System.IO ( IO, hPutStrLn, stderr )
import qualified System.FilePath as FP

import HLinear.Bench.MatrixParser ( matrixFMPQParser )
import HLinear.Matrix ( Matrix )
import qualified HLinear.NormalForm.FoldUnfold.PLE.DivisionRing as PLEDR
import qualified HLinear.NormalForm.FoldUnfold.PLE.FractionFree as PLEFF
import qualified HLinear.NormalForm.FoldUnfold.ReduceEchelonForm.DivisionRing as RREF


inputFileNameOption :: Parser String
inputFileNameOption =
  argument str (metavar "INPUT_FILENAME")


main :: IO ()
main =
  let opts = info (inputFileNameOption <**> helper)
               ( fullDesc
              <> progDesc "Benchmark HLinear for matrix in INPUT_FILENAME"
              <> header "bench-hlinear" )
  in do
    inputFileName <- execParser opts
    matrixString <- readFile inputFileName
    case parseOnly matrixFMPQParser matrixString of
      Left err -> hPutStrLn stderr $ "Parse error of '" <> inputFileName <> "': " <> err
      Right mat -> mainBenchmark inputFileName mat

mainBenchmark :: FilePath -> Matrix FMPQ -> IO ()
mainBenchmark fileName mat =
  defaultMainWith
  defaultConfig { jsonFile = Just $ fileName FP.<.> "json" }
    [ bench "ple classical"     $ nf PLEDR.ple mat
    , bench "ple fraction free" $ nf PLEFF.ple mat
    , bench "rref" $ nf RREF.rref mat
    ]
