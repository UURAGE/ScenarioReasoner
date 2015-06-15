module Domain.Scenarios.Exercises where

import Control.Monad
import Data.List
import Data.Map(findWithDefault, fromList, empty)
import Data.Maybe(fromMaybe)
import System.Directory
import System.FilePath(FilePath, takeBaseName)
import System.FilePath.Find as F

import Ideas.Common.Library

import Domain.Scenarios.Globals
import Domain.Scenarios.Strategy(makeStrategy)
import Domain.Scenarios.Parser(parseScript, parseScenario)
import Domain.Scenarios.Scenario
import Domain.Scenarios.ScenarioState

exercises :: IO [(Exercise ScenarioState, FilePath)]
exercises = 
    F.find F.always (F.extension ==? ".xml") root >>= return . map readExercise 
  where root = "../../scenarios/scripts" :: FilePath-- : The script directory.

readExercise :: FilePath -> (Exercise ScenarioState, FilePath)
readExercise path = (mkExercise id strategy difficulty initialState, path)
  where 
    id = "scenarios" # newId (takeBaseName path)    
    script = parseScript path
    scenario@(Scenario metadata _ dialogue) = (parseScenario script)
    strategy = makeStrategy (scenarioID metadata) dialogue
    difficulty = scenarioDifficulty metadata
    parameters = scenarioParameters metadata
    processParameter p = (parameterId p, fromMaybe 0 (parameterInitialValue p))
    initialState = ScenarioState (fromList (map processParameter parameters)) empty emptyStatementInfo
    
mkExercise :: Id -> Strategy ScenarioState -> Difficulty -> ScenarioState -> Exercise ScenarioState
mkExercise id strat difficulty initState = 
    makeExercise
       { exerciseId     = id
       , status         = Alpha
       , parser         = readJSON
       , prettyPrinter  = showJSON
       , equivalence    = \_ _-> True
       , similarity     = \_ _-> True
       , ready          = true
       , suitable       = true
       , hasTypeable    = useTypeable
       , strategy       = liftToContext $ label "Scenario Strategy" strat
       , examples       = [(difficulty, initState)]
       }