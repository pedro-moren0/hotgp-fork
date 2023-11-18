{-# LANGUAGE ScopedTypeVariables #-}

module Benchmark.Run where

import Benchmark.BenchmarkToConfig (makeConfig, makeConfigTask1)
import Benchmark.Core
import Benchmark.Dataset
import Benchmark.Download (csvDirs, downloadBenchmark)
import Benchmark.Helpers
import Benchmark.Log
import Control.Monad (unless, when)
import Control.Monad.State.Strict (evalState, evalStateT)
import qualified Data.ByteString.Lazy as B
import Data.Either (fromRight)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.SortedList (fromSortedList)
import qualified Data.SortedList as SL
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.Vector as V
import Evolution
import Evolution.Fitness (Fitness)
import Evolution.Individual (SortedPop)
import Grammar
import Pretty
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, removeFile)
import System.IO
import System.Random (mkStdGen)

runBenchmark :: (Fitness a, Monoid a, Show a) => FilePath -> Int -> Benchmark a -> IO ()
runBenchmark workDir seed benchmark = do
  hSetBuffering stdout $ BlockBuffering Nothing
  identifier <- logFileIdentifier benchmark seed
  (logFileHandle, finalResultFile, jsonFile) <- prepareLogFiles identifier workDir benchmark
  (trainSet, testSet) <- loadTrainAndTestSet workDir benchmark

  let cfg = makeConfig trainSet benchmark
      checkpoint = checkpointFilename workDir seed benchmark
      logToFile cache time evals pop = do
        let (row, newCache) = logPop cache testSet time evals pop
        hPutStrLn logFileHandle row
        when (evals `mod` 100 == 0) $ hFlush logFileHandle
        return newCache

  (finalPop, nEvals) <- evalStateT (runAndLog cfg logToFile checkpoint) $ mkStdGen seed

  -- putStrLn $ pp $ finalPop

  hClose logFileHandle
  writeFile finalResultFile $ logBest testSet (_indTree $ head $ SL.fromSortedList finalPop)
  writeFile jsonFile $ jsonBest (getBenchmarkId benchmark) seed nEvals testSet (head $ SL.fromSortedList finalPop)

runBenchmarkTask1 :: (Fitness a, Monoid a, Show a) => FilePath -> Int -> Int -> Int -> Benchmark a -> IO ()
runBenchmarkTask1 workDir popSize evalNumber seed benchmark = do
  hSetBuffering stdout $ BlockBuffering Nothing
  identifier <- logFileIdentifierTask1 benchmark popSize evalNumber seed
  (logFileHandle, finalResultFile, jsonFile) <- prepareLogFiles identifier workDir benchmark
  (trainSet, testSet) <- loadTrainAndTestSet workDir benchmark

  let cfg = makeConfigTask1 popSize evalNumber trainSet benchmark
      checkpoint = checkpointFilenameTask1 workDir popSize evalNumber seed benchmark
      logToFile cache time evals pop = do
        let (row, newCache) = logPop cache testSet time evals pop
        hPutStrLn logFileHandle row
        when (evals `mod` 100 == 0) $ hFlush logFileHandle
        return newCache

  (finalPop, nEvals) <- evalStateT (runAndLog cfg logToFile checkpoint) $ mkStdGen seed

  -- putStrLn $ pp $ finalPop

  hClose logFileHandle
  writeFile finalResultFile $ logBest testSet (_indTree $ head $ SL.fromSortedList finalPop)
  writeFile jsonFile $ jsonBest (getBenchmarkId benchmark) seed nEvals testSet (head $ SL.fromSortedList finalPop)

logFileIdentifier :: Benchmark a -> Int -> IO String
logFileIdentifier benchmark seed = do
  time <- getCurrentTime
  return $ show time <> "_s" <> show seed <> "_" <> getBenchmarkId benchmark

logFileIdentifierTask1 :: Benchmark a -> Int -> Int -> Int -> IO String
logFileIdentifierTask1 benchmark popSize evalNum seed = do
  return $ "p" <> show popSize <> "_e" <> show evalNum <> "_s" <> show seed <> "_" <> getBenchmarkId benchmark

checkpointFilename :: FilePath -> Int -> Benchmark a -> FilePath
checkpointFilename _ seed benchmark = "./checkpoint/" <> "s" <> show seed <> "_" <> getBenchmarkId benchmark <> ".checkpoint"

checkpointFilenameTask1 :: FilePath -> Int -> Int -> Int -> Benchmark a -> FilePath
checkpointFilenameTask1 _ pop eval seed benchmark =
  "./checkpoint/"
    <> "p"
    <> show pop
    <> "_e"
    <> show eval
    <> "_s"
    <> show seed
    <> "_"
    <> getBenchmarkId benchmark
    <> ".checkpoint"
