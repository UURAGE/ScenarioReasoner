{- © Utrecht University (Department of Information and Computing Sciences) -}

module Main where

import Control.Monad

import Data.Binary

import Domain.Scenarios.Parser

import System.Environment
import System.FilePath
import System.IO

import Ideas.Common.Id
import Ideas.Text.XML
import Ideas.Text.XML.Unicode

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
    scenarioXML <- parseXMLFileFixed xml_path
    let scenario = parseScenario scenarioXML
    encodeFile bin_path scenario
    return (showId (newId (takeBaseName bin_path)))

parseXMLFileFixed :: FilePath -> IO XML
parseXMLFileFixed file =
    withBinaryFile file ReadMode $
        hGetContents >=> decoding >=> either fail return . parseXML

debugMain :: String -> IO ()
debugMain path = do
    scenarioXML <- parseXMLFile path
    let scenario = parseScenario scenarioXML
    print scenario
    print path
    encodeFile ("bins/" ++ takeBaseName path ++ ".bin") scenario
