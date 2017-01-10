{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Main where

import Data.Binary
import Data.Functor

import Domain.Scenarios.Parser

import System.Environment
import System.FilePath
import System.IO

import Ideas.Common.Id

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-r", script_path, bin_path] -> do
            scenarioID <- process script_path bin_path
            hSetEncoding stdout utf8
            putStrLn scenarioID
        [script_path, bin_path] -> void (process script_path bin_path)
        _ -> error "Not enough arguments"

process :: FilePath -> FilePath -> IO String
process script_path bin_path = do
    script <- parseScript script_path
    let scenario = parseScenario script
    encodeFile bin_path scenario
    return (showId (newId (takeBaseName bin_path)))

debugMain :: String -> IO ()
debugMain path = do
    script <- parseScript path
    let scenario = parseScenario script
    print scenario
    print path
    encodeFile ("bins/" ++ takeBaseName path ++ ".bin") scenario
