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

import System.FilePath

main :: IO ()
main = runCGI $ handleErrors $ do
    script_path <- getInput "script_path"
    script <- liftIO $ parseScript (fromJust script_path)
    let scenario = parseScenario script
    bin_path <- getInput "bin_path"
    liftIO $ encodeFile (fromJust bin_path) scenario
    output (fromJust bin_path)
    
debugMain :: String -> IO ()
debugMain path = do
    script <- parseScript (path)
    let scenario = parseScenario script
    print scenario
    print path
    encodeFile ("bins/" ++ takeBaseName path ++ ".bin") scenario
	