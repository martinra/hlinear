{-# LANGUAGE
    FlexibleInstances
  , ScopedTypeVariables
  #-}

module Main
where

import HLinear.Utility.Prelude

import Criterion.Main
import Criterion.Types ( Config(..) )
import Data.Attoparsec.ByteString.Char8 ( parseOnly )
import Data.ByteString ( readFile )
import Options.Applicative
import System.FilePath ( FilePath )
import System.IO ( IO, hPutStrLn, stderr )
import qualified Criterion.Main.Options as Criterion
import qualified System.FilePath as FP

import HLinear.Bench.MatrixParser ( parserMatrixFMPQ )
import HLinear.Matrix ( Matrix )
import qualified HLinear.NormalForm.FoldUnfold.PLE.DivisionRing as PLEDR
import qualified HLinear.NormalForm.FoldUnfold.PLE.FractionFree as PLEFF
import qualified HLinear.NormalForm.FoldUnfold.ReduceEchelonForm.DivisionRing as RREF


main :: IO ()
main =
  let opts = info
               ( argument str (metavar "INPUT_FILENAME")
            <**> helper )
               ( fullDesc
              <> progDesc "Benchmark HLinear for matrix in INPUT_FILENAME"
              <> header "bench-hlinear" )
  in do
    inputFileName <- execParser opts
    matrixString <- readFile inputFileName
    case parseOnly parserMatrixFMPQ matrixString of
      Left err -> hPutStrLn stderr $ "Parse error of '" <> inputFileName <> "': " <> err
      Right mat -> mainBenchmark inputFileName mat

mainBenchmark :: FilePath -> Matrix FMPQ -> IO ()
mainBenchmark fileName mat =
  let cfg = defaultConfig { jsonFile = Just $ fileName FP.<.> "json" }
  in  runMode (Criterion.Run cfg Criterion.Prefix [])
        [ bench "ple classical"     $ nf PLEDR.ple mat
        , bench "ple fraction free" $ nf PLEFF.ple mat
        , bench "rref" $ nf RREF.rref mat
        ]
