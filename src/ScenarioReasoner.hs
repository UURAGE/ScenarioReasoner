{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Main where

-- Own imports
import System.Environment
import System.FilePath (takeBaseName)
import System.FilePath.Find as F
import Ideas.Common.Id
import Ideas.Common.Library (Some(..))
import Ideas.Main.Default (defaultCGI)
import Domain.Scenarios.Scenario
import Domain.Scenarios.Services.ServiceList
import qualified Domain.Scenarios.Exercises as E

-- Imports used by Ideas.Main.Default, minus the superfluous ones
import Control.Exception
import Data.Maybe
import Ideas.Encoding.ModeJSON (processJSON)
import Ideas.Encoding.ModeXML (processXML)
import Ideas.Encoding.Options (Options, maxTime)
import Ideas.Encoding.Request
import Ideas.Service.DomainReasoner
import Network.CGI
import System.IO
import System.IO.Error (ioeGetErrorString)
import qualified Ideas.Encoding.Logging as Log
import qualified Ideas.Main.CmdLineOptions as Options

main :: IO ()
main = do
   sr <- scenarioReasoner
   args <- getArgs
   case args of
      "-r" : _ -> scenarioReasonerCommandLine sr
      _ -> defaultCGI mempty sr

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
   (_, txtOut, _) <- process mempty dr logRef txtIn
   putStrLn txtOut

process :: Options -> DomainReasoner -> Log.LogRef -> String -> IO (Request, String, String)
process options dr logRef input = do
   format <- discoverDataFormat input
   run format options {maxTime = Just 5} (addVersion dr) logRef input
 `catch` \ioe -> do
   let msg = "Error: " ++ ioeGetErrorString ioe
   Log.changeLog logRef (\r -> r { Log.errormsg = msg })
   return (mempty, msg, "text/plain")
 where
   run XML  = processXML
   run JSON = processJSON

makeTestRunner :: DomainReasoner -> String -> IO String
makeTestRunner dr input = do
   (_, out, _) <- process mempty dr Log.noLogRef input
   return out

addVersion :: DomainReasoner -> DomainReasoner
addVersion dr = dr
   { version     = update version Options.shortVersion
   , fullVersion = update fullVersion Options.fullVersion
   }
 where
   update f s = if null (f dr) then s else f dr
