------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

module Main where

import Data.Binary
import Data.Maybe

import Network.CGI

import Domain.Scenarios.Parser

import System.Environment
import System.FilePath

main :: IO ()
main = do
    args <- getArgs
    case args of
        [script_path, bin_path] -> process script_path bin_path
        _ -> runCGI $ handleErrors $ do
            script_path <- getInput "script_path"
            bin_path <- getInput "bin_path"
            process (fromJust script_path) (fromJust bin_path)
            output (fromJust bin_path)

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
