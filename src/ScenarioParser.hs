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
        ["-r", xml_path, bin_path] -> do
            scenarioID <- process xml_path bin_path
            hSetEncoding stdout utf8
            putStrLn scenarioID
        [xml_path, bin_path] -> void (process xml_path bin_path)
        _ -> error "Not enough arguments"

process :: FilePath -> FilePath -> IO String
process xml_path bin_path = do
    scenarioXML <- parseScript xml_path
    let scenario = parseScenario scenarioXML
    encodeFile bin_path scenario
    return (showId (newId (takeBaseName bin_path)))

debugMain :: String -> IO ()
debugMain path = do
    scenarioXML <- parseScript path
    let scenario = parseScenario scenarioXML
    print scenario
    print path
    encodeFile ("bins/" ++ takeBaseName path ++ ".bin") scenario
