module Domain.Scenarios.Exercises where

import Control.Monad
import Data.List
import Data.Map(findWithDefault, fromList, empty)
import Data.Maybe(fromMaybe)
import System.Directory

import Ideas.Common.Library

import Domain.Scenarios.Globals
import Domain.Scenarios.Strategy(makeStrategy)
import Domain.Scenarios.Parser(parseScript, parseScenario)
import Domain.Scenarios.Scenario
import Domain.Scenarios.ScenarioState

getExercises :: String ->  IO ([Exercise ScenarioState], [Script])
getExercises scenarioId = do
    let scriptPath = "../../scenarios/scripts/" ++ tail (dropWhile (/= '.') scenarioId) ++ ".xml" -- : The script directory.
    (exercise, script) <- getExercise scriptPath
    return (dummyExercise : [exercise], [script])

getExercise :: String -> IO (Exercise ScenarioState, Script)
getExercise path = do
    script <- parseScript path
    exercise <- exerciseFromScript script
    return (exercise, script)

exerciseFromScript :: Monad m => Script -> m (Exercise ScenarioState)
exerciseFromScript script = do
    let scenario@(Scenario metadata dialogue) = (parseScenario script)
        scenarioStrategy = makeStrategy (scenarioID metadata) dialogue
        difficulty = scenarioDifficulty metadata
        parameters = scenarioParameters metadata
        processParameter p = (parameterId p, fromMaybe 0 (parameterInitialValue p))
        initialState = ScenarioState (fromList (map processParameter parameters)) empty emptyStatementInfo 
    return makeExercise
       { exerciseId     = getId scenario
       , status         = Alpha
       , parser         = readJSON
       , prettyPrinter  = showJSON
       , equivalence    = \_ _-> True
       , similarity     = \_ _-> True
       , ready          = true
       , suitable       = true
       , hasTypeable    = useTypeable
       -- , extraRules     = undefined
       , strategy       = liftToContext $ label "The One And Only Strategy" scenarioStrategy
       -- , navigation     = undefined
       , testGenerator  = Nothing
       -- , randomExercise = undefined
       , examples = [(difficulty, initialState)]
       }
       
-- A dummy exercise necessary for use with general services
dummyExercise :: Exercise ScenarioState
dummyExercise = makeExercise { exerciseId = newId "scenarios-dummy" }
