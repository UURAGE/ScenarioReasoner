{- © Utrecht University (Department of Information and Computing Sciences) -}

module Main where

-- Own imports
import System.Environment
import System.FilePath (takeBaseName)
import System.FilePath.Find as F
import Ideas.Common.Id
import Ideas.Common.Library (Some(..))
import Ideas.Encoding.Options (baseUrl)
import Ideas.Main.Default (defaultCGI)
import Domain.Scenarios.Scenario
import Domain.Scenarios.Services.ServiceList
import qualified Domain.Scenarios.Exercises as E

-- Imports used by Ideas.Main.Default, minus the superfluous ones
import Control.Exception
import Ideas.Encoding.ModeJSON (processJSON)
import Ideas.Encoding.ModeXML (processXML)
import Ideas.Encoding.Options (Options, maxTime, logRef)
import Ideas.Encoding.Request
import Ideas.Service.DomainReasoner
import Ideas.Text.XML.Unicode (decoding)
import System.IO
import qualified Ideas.Encoding.Logging as Log
import qualified Ideas.Main.CmdLineOptions as Options

main :: IO ()
main = do
   sr <- scenarioReasoner
   args <- getArgs
   case args of
      "-r" : _ -> scenarioReasonerCommandLine sr
      _ -> defaultCGI (mempty { baseUrl = Just "media/" }) sr

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

-- Invoked from command-line using raw mode
scenarioReasonerCommandLine :: DomainReasoner -> IO ()
scenarioReasonerCommandLine dr = do
   mapM_ (`hSetBinaryMode` True) [stdin, stdout, stderr]
   txtIn          <- getContents >>= decoding
   (_, txtOut, _) <- process mempty dr txtIn
   putStrLn txtOut

process :: Options -> DomainReasoner -> String -> IO (Request, String, String)
process options dr input = do
   format <- discoverDataFormat input
   run format options {maxTime = Just 5} (addVersion dr) input
 `catch` \e -> do
   let msg = "Error: " ++ show (e :: SomeException)
   Log.changeLog (logRef options) (\r -> r { Log.errormsg = msg })
   return (mempty, msg, "text/plain")
 where
   run XML  = processXML
   run JSON = processJSON

addVersion :: DomainReasoner -> DomainReasoner
addVersion dr = dr
   { version     = update version Options.shortVersion
   , fullVersion = update fullVersion Options.fullVersion
   }
 where
   update f s = if null (f dr) then s else f dr
