{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Main where

import Data.Binary
import Data.Functor
import Data.Maybe

import Network.CGI

import Domain.Scenarios.Parser

import System.Environment
import System.FilePath
import System.IO

import Ideas.Common.Id
import Ideas.Text.UTF8 as UTF8

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-r", script_path, bin_path] -> do
            scenarioID <- process script_path bin_path
            hSetEncoding stdout utf8
            putStrLn scenarioID
        [script_path, bin_path] -> void (process script_path bin_path)
        _ -> runCGI $ handleErrors $ do
            script_path <- getInput "script_path"
            bin_path <- getInput "bin_path"
            scenarioID <- process (fromJust script_path) (fromJust bin_path)
            setHeader "Content-Type" "text/plain; charset=utf-8"
            output (UTF8.encode scenarioID)

process :: MonadIO m => FilePath -> FilePath -> m String
process script_path bin_path = do
    script <- liftIO $ parseScript script_path
    let scenario = parseScenario script
    liftIO $ encodeFile bin_path scenario
    return (showId (newId (takeBaseName bin_path)))

debugMain :: String -> IO ()
debugMain path = do
    script <- parseScript path
    let scenario = parseScenario script
    print scenario
    print path
    encodeFile ("bins/" ++ takeBaseName path ++ ".bin") scenario
