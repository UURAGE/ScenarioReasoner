{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Main where

import Data.Binary
import Data.Maybe

import Network.CGI

import Domain.Scenarios.Parser

import System.Environment
import System.FilePath

import Ideas.Common.Id
import Ideas.Text.UTF8 as UTF8

main :: IO ()
main = do
    args <- getArgs
    case args of
        [script_path, bin_path] -> process script_path (makeValidFilePath bin_path)
        _ -> runCGI $ handleErrors $ do
            script_path <- getInput "script_path"
            bin_path <- getInput "bin_path"
            let valid_bin_path = makeValidFilePath (fromJust bin_path)
            process (fromJust script_path) valid_bin_path
            output $ UTF8.encode valid_bin_path

makeValidFilePath :: FilePath -> String
makeValidFilePath bin_path = replaceBaseName bin_path validFileName
  where validFileName = showId (newId (takeBaseName bin_path))

process :: MonadIO m => FilePath -> FilePath -> m ()
process script_path bin_path = do
    script <- liftIO $ parseScript script_path
    let scenario = parseScenario script
    liftIO $ encodeFile bin_path scenario

debugMain :: String -> IO ()
debugMain path = do
    script <- parseScript (path)
    let scenario = parseScenario script
    print scenario
    print path
    encodeFile ("bins/" ++ takeBaseName path ++ ".bin") scenario
