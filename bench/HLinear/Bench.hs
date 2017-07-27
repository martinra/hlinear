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
import HLinear.Hook.PLEHook ( PLUEHook(..) )
import HLinear.Matrix ( Matrix )
import HLinear.NormalForm.FoldUnfold.RREF.DivisionRing ( rrefWithPLE )
import qualified HLinear.NormalForm.FoldUnfold.PLE.DivisionRing as PLEDR
import qualified HLinear.NormalForm.FoldUnfold.PLE.FractionFree as PLEFF


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
  let cfg = defaultConfig
              { timeLimit = 600
              , jsonFile = Just $ fileName FP.<.> "criterion" FP.<.> "json"
              }
      getEchelonForm (PLUEHook _ _ _ e) = e
  in  runMode (Criterion.Run cfg Criterion.Prefix [])
        [ bench "rref classical"     $ nf (getEchelonForm . rrefWithPLE PLEDR.ple) mat
        , bench "rref fraction free" $ nf (getEchelonForm . rrefWithPLE PLEFF.ple) mat
        ]
