------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

module Main where

import Control.Arrow

import System.Environment

import Data.Binary
import Data.Maybe

import Ideas.Common.Exercise
import Ideas.Common.Id
import Ideas.Common.Utils (Some(..))
import Ideas.Common.Utils.TestSuite
import Ideas.Common.Library
import Ideas.Main.Default
import Ideas.Main.Documentation
import Ideas.Service.DomainReasoner
import Ideas.Service.ServiceList
import Ideas.Service.Types (Service)

import Network.CGI

import Domain.Scenarios.Parser

import System.FilePath

main :: IO ()
main = runCGI $ handleErrors $ do
    path <- getInput "path"
    script <- liftIO $ parseScript (fromJust path)
    let scenario = parseScenario script
    liftIO $ encodeFile ("bins/" ++ takeBaseName (fromJust path)) scenario
    output (fromJust path)
	