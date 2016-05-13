{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Exercises where

import Data.Map(fromList)
import Data.Maybe(fromMaybe)
import System.FilePath(takeBaseName)
import System.FilePath.Find as F

import Ideas.Common.Library

import Domain.Scenarios.Globals
import Domain.Scenarios.Strategy(makeStrategy)
import Domain.Scenarios.Scenario
import Domain.Scenarios.ScenarioState

exercises :: IO [(Exercise ScenarioState, FilePath)]
exercises = 
    F.find F.always (F.extension ==? ".bin") root >>= return . map readExercise
  where root = "bins" :: FilePath
  
testingExercises :: IO [(Exercise ScenarioState, FilePath)]
testingExercises = 
    F.find F.always (F.extension ==? ".bin") root >>= return . map readExercise 
  where root = "test_bins" :: FilePath

readExercise :: FilePath -> (Exercise ScenarioState, FilePath)
readExercise path = (mkExercise sId strat difficulty initialState, path)
  where 
    sId = newId (takeBaseName path)
    Scenario metadata _ dialogue = readBinaryScenario path
    strat      = makeStrategy (showId sId) dialogue
    difficulty = scenarioDifficulty metadata
    parameters = scenarioParameters metadata
    processParameter p = (parameterId p, fromMaybe 0 (parameterInitialValue p))
    initialState = ScenarioState (fromList (map processParameter parameters)) Nothing

mkExercise :: Id -> Strategy ScenarioState -> Maybe Difficulty -> ScenarioState -> Exercise ScenarioState
mkExercise sId strat difficulty initState = 
    emptyExercise
       { exerciseId     = sId
       , status         = Alpha
       , parser         = readJSON
       , prettyPrinter  = showJSON
       , equivalence    = \_ _-> True
       , similarity     = \_ _-> True
       , ready          = true
       , suitable       = true
       , hasTermView    = Nothing
       , hasTypeable    = useTypeable
       , strategy       = liftToContext $ label "Scenario Strategy" strat
       , examples       = [(fromMaybe Medium difficulty, initState)]
       }
