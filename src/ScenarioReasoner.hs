------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

module Main where

import Ideas.Main.Documentation
import Domain.Scenarios.Scenario
import System.FilePath (takeBaseName)
import System.FilePath.Find as F
import qualified Domain.Scenarios.Services.ServiceList as S
import qualified Domain.Scenarios.Exercises as E

import Control.Exception
import Control.Monad
import Data.Maybe
import Ideas.Common.Id
import Ideas.Common.Utils (Some(..))
import Ideas.Encoding.ModeJSON (processJSON)
import Ideas.Encoding.ModeXML (processXML)
import Ideas.Main.Options hiding (fullVersion)
import Ideas.Service.DomainReasoner
import Ideas.Service.Request
import Ideas.Service.ServiceList
import Network.CGI
import Prelude hiding (catch)
import System.IO.Error (ioeGetErrorString)
import qualified Ideas.Main.Logging as Log

main :: IO ()
main = scenarioReasoner >>= scenarioReasonerCGI

maindoc :: IO ()
maindoc = do
    drTuple <- scenarioReasoner
    let dr = fst drTuple
    makeDocumentation dr "doc"

scenarioReasoner :: IO (DomainReasoner, DomainReasoner)
scenarioReasoner = do
    -- Filter the serviceList of Ideas, because we have our own allfirsts in adapted services
    let filteredServiceList = filter (\s -> getId s /= "basic" # "allfirsts") serviceList

    iss <- readBinaryScenarios "bins"
    let dr  = (newDomainReasoner "ideas.scenarios")
            { exercises = map Some (E.exercises iss)
            , services  = S.customServices iss ++ metaServiceList dr ++ filteredServiceList
            }

    tiss <- readBinaryScenarios "test_bins"
    let tdr = (newDomainReasoner "ideas.scenarios.test")
            { exercises = map Some (E.exercises tiss)
            , services  = S.customServices tiss ++ metaServiceList dr ++ filteredServiceList
            }

    return (dr, tdr)

readBinaryScenarios :: FilePath -> IO [(Id, Scenario)]
readBinaryScenarios root = do
    paths <- F.find F.always (F.extension ==? ".bin") root
    let readOne path = ("scenarios" # newId (takeBaseName path), readBinaryScenario path)
    return (map readOne paths)

-- Invoked as a cgi binary
scenarioReasonerCGI :: (DomainReasoner, DomainReasoner) -> IO ()
scenarioReasonerCGI (sr, srt) = runCGI $ handleErrors $ do
   -- create a record for logging
   logRef  <- liftIO Log.newLogRef
   -- query environment
   addr    <- remoteAddr       -- the IP address of the remote host
   cgiBin  <- scriptName       -- get name of binary
   input   <- inputOrDefault

   -- create a domain reasoner based on testing or not
   testingInput <- getInput "testing"
   testing <- case testingInput of
                Just t  -> return (read t :: Bool)
                Nothing -> return False
   let dr = if testing then srt else sr

   -- process request
   (req, txt, ctp) <- liftIO $
      process dr logRef (Just cgiBin) input
   -- log request to database
   when (useLogging req) $ liftIO $ do
      Log.changeLog logRef $ \r -> Log.addRequest req r
         { Log.ipaddress = addr
         , Log.version   = shortVersion
         , Log.input     = input
         , Log.output    = txt
         }
      Log.logRecord (getSchema req) logRef

   -- write header and output
   setHeader "Content-type" ctp
   -- Cross-Origin Resource Sharing (CORS) prevents browser warnings
   -- about cross-site scripting
   setHeader "Access-Control-Allow-Origin" "*"
   output txt

inputOrDefault :: CGI String
inputOrDefault = do
   inHtml <- acceptsHTML
   ms     <- getInput "input" -- read variable 'input'
   case ms of
      Just s -> return s
      Nothing
         | inHtml    -> return defaultBrowser
         | otherwise -> fail "environment variable 'input' is empty"
 where
   -- Invoked from browser
   defaultBrowser :: String
   defaultBrowser = "<request service='index' encoding='html'/>"

   acceptsHTML :: CGI Bool
   acceptsHTML = do
      maybeAcceptCT <- requestAccept
      let htmlCT = ContentType "text" "html" []
          xs = negotiate [htmlCT] maybeAcceptCT
      return (isJust maybeAcceptCT && not (null xs))

process :: DomainReasoner -> Log.LogRef -> Maybe String -> String -> IO (Request, String, String)
process dr logRef cgiBin input = do
   format <- discoverDataFormat input
   run format (Just 5) cgiBin dr logRef input
 `catch` \ioe -> do
   let msg = "Error: " ++ ioeGetErrorString ioe
   Log.changeLog logRef (\r -> r { Log.errormsg = msg })
   return (emptyRequest, msg, "text/plain")
 where
   run XML  = processXML
   run JSON = processJSON
