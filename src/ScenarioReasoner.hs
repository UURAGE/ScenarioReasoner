{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Main where

import System.Environment
import System.IO
import System.FilePath (takeBaseName)
import System.FilePath.Find as F
import Ideas.Main.Documentation
import Domain.Scenarios.Scenario
import Domain.Scenarios.Services.ServiceList
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
import Network.CGI
import System.IO.Error (ioeGetErrorString)
import qualified Ideas.Main.Logging as Log

main :: IO ()
main = do
   sr <- scenarioReasoner
   args <- getArgs
   case args of
      "-r" : _ -> scenarioReasonerCommandLine sr
      _ -> scenarioReasonerCGI sr

maindoc :: IO ()
maindoc = do
    dr <- scenarioReasoner
    makeDocumentation dr "doc"

scenarioReasoner :: IO DomainReasoner
scenarioReasoner = do
    iss <- readBinaryScenarios "bins"
    let dr  = (newDomainReasoner "ideas.scenarios")
            { exercises = map Some (E.exercises iss)
            , services  = customServiceList iss ++ metaServiceList dr ++ serviceList
            }

    return dr

readBinaryScenarios :: FilePath -> IO [(Id, Scenario)]
readBinaryScenarios root = do
    paths <- F.find F.always (F.extension ==? ".bin") root
    let readOne path = (newId (takeBaseName path), readBinaryScenario path)
    return (map readOne paths)

-- Invoked as a cgi binary
scenarioReasonerCGI :: DomainReasoner -> IO ()
scenarioReasonerCGI dr = runCGI $ handleErrors $ do
   -- create a record for logging
   logRef  <- liftIO Log.newLogRef
   -- query environment
   addr    <- remoteAddr       -- the IP address of the remote host
   cgiBin  <- scriptName       -- get name of binary
   input   <- inputOrDefault

   -- process request
   (req, txt, ctp) <- liftIO $
      process dr logRef (Just cgiBin) input
    `catch` \ioe -> do
      let msg = "Error: " ++ ioeGetErrorString ioe
      Log.changeLog logRef (\r -> r { Log.errormsg = msg })
      return (emptyRequest, msg, "text/plain")

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

-- Invoked from command-line using raw mode
scenarioReasonerCommandLine :: DomainReasoner -> IO ()
scenarioReasonerCommandLine dr = do
   mapM_ (`hSetBinaryMode` True) [stdin, stdout, stderr]
   txtIn          <- getContents
   logRef         <- liftIO Log.newLogRef
   (_, txtOut, _) <- process dr logRef Nothing txtIn
   putStrLn txtOut

process :: DomainReasoner -> Log.LogRef -> Maybe String -> String -> IO (Request, String, String)
process dr logRef cgiBin input = do
   format <- discoverDataFormat input
   run format (Just 5) cgiBin dr logRef input
 where
   run XML  = processXML
   run JSON = processJSON
