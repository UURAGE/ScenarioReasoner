{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2013, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Main module for feedback services
--
-----------------------------------------------------------------------------
module Ideas.Main.Default
   ( defaultMain, newDomainReasoner
     -- extra exports
   , Some(..), serviceList, metaServiceList, Service
   , module Ideas.Service.DomainReasoner
   ) where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Time
import Data.List
import Ideas.Common.Id
import Ideas.Common.Utils (useFixedStdGen, Some(..))
import Ideas.Common.Utils.TestSuite
import Ideas.Encoding.ModeJSON (processJSON)
import Ideas.Encoding.ModeXML (processXML)
import Ideas.Main.BlackBoxTests
import Ideas.Main.Documentation
import Ideas.Main.LoggingDatabase
import Ideas.Main.Options hiding (fullVersion)
import Ideas.Service.DomainReasoner
import Ideas.Service.ServiceList
import Ideas.Service.Types (Service)
import Ideas.Service.FeedbackScript.Analysis
import Ideas.Service.Request
import Network.CGI
import Prelude hiding (catch)
import System.IO
import System.IO.Error (ioeGetErrorString)
import qualified Ideas.Main.Options as Options

defaultMain :: (String -> IO DomainReasoner) -> IO ()
defaultMain dr = do
   startTime <- getCurrentTime
   flags     <- getFlags
   if null flags
      then defaultCGI dr startTime
      else error("not supported anymore") --defaultCommandLine dr flags

-- Invoked as a cgi binary
defaultCGI :: (String -> IO DomainReasoner) -> UTCTime -> IO ()
defaultCGI dr startTime = do
   logRef <- newIORef (return ())
   runCGI $ do
      addr   <- remoteAddr       -- the IP address of the remote host making the request
      cgiBin <- scriptName       -- get name of binary
      raw    <- getInput "input" -- read input
      input  <- case raw of
                   Nothing -> fail "Invalid request: environment variable \"input\" is empty"
                   Just s  -> return s

      let scenId = getScenarioId input
      reasoner <- liftIO $ dr (fromJust scenId)
      (req, txt, ctp) <- liftIO $ process reasoner (Just cgiBin) input

      -- save logging action for later
      when (useLogging req) $
         liftIO $ writeIORef logRef $
            logMessage req input txt addr startTime
      setHeader "Content-type" ctp
      -- Cross-Origin Resource Sharing (CORS) prevents browser warnings
      -- about cross-site scripting
      setHeader "Access-Control-Allow-Origin" "*"
      output txt
   -- log request to database
   join (readIORef logRef)
   -- if something goes wrong
 `catch` \ioe -> runCGI $ do
   setHeader "Content-type" "text/plain"
   setHeader "Access-Control-Allow-Origin" "*"
   output ("Invalid request\n" ++ ioeGetErrorString ioe)
{--
-- Invoked from command-line with flags
defaultCommandLine :: (String -> IO DomainReasoner) -> [Flag] -> IO ()
defaultCommandLine dr flags = do
   hSetBinaryMode stdout True
   useFixedStdGen -- always use a predictable "random" number generator
   mapM_ doAction flags
 where
   doAction flag =
      case flag of
         -- information
         Version -> putStrLn ("IDEAS, " ++ versionText)
         Help    -> putStrLn helpText
         -- process input file
         InputFile file -> do
            input <- readFile file
            (_, txt, _) <- process dr Nothing input
            putStrLn txt
         -- blackbox tests
         Test dir -> do
            tests  <- blackBoxTests dr dir
            result <- runTestSuiteResult True tests
            printSummary result
         -- generate documentation pages
         MakePages dir ->
            makeDocumentation dr dir
         -- feedback scripts
         MakeScriptFor s    -> makeScriptFor dr s
         AnalyzeScript file -> parseAndAnalyzeScript dr file
         --}

process :: DomainReasoner -> Maybe String -> String -> IO (Request, String, String)
process dr cgiBin input =
   case discoverDataFormat input of
      Just XML  -> processXML (Just 5) dr cgiBin input
      Just JSON -> processJSON (Just 5) (isJust cgiBin) dr input
      _ -> fail "Invalid input"

newDomainReasoner :: IsId a => a -> DomainReasoner
newDomainReasoner a = mempty
   { reasonerId  = newId a
   , version     = shortVersion
   , fullVersion = Options.fullVersion
   }

-- | this method gets the scenario id from the cgi input string. necessary to prevent parsing every scenario for every request
getScenarioId :: String -> Maybe String
getScenarioId input = case index of
                  Just foundIndex -> Just $ takeWhile (\x -> x /= '\"') $ drop (foundIndex+11) input --10 is length of hardcoded string the precedes the scenrio id
                  Nothing -> Nothing
                  where index = subStringIndex "params\":[[\"" input

subStringIndex :: String -> String -> Maybe Int
subStringIndex part whole = elemIndex True $ map (subStrIndHelper part) (tails whole)

subStrIndHelper :: String -> String -> Bool
subStrIndHelper [] [] = True
subStrIndHelper [] whole = True
subStrIndHelper part [] = False
subStrIndHelper (y:ys) (x:xs)
                  | y == x = subStrIndHelper ys xs
                  | otherwise = False