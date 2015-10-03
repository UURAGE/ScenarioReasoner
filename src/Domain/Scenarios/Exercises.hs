------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

module Domain.Scenarios.Exercises where

import Control.Monad
import Data.List hiding (insert)
import Data.Map(insert, findWithDefault, fromList, empty)
import Data.Maybe(fromMaybe)
import System.Directory
import System.FilePath(FilePath, takeBaseName)
import System.FilePath.Find as F

import Ideas.Common.Library

import Domain.Scenarios.Globals
import Domain.Scenarios.Strategy(makeStrategy)
import Domain.Scenarios.Parser
import Domain.Scenarios.Scenario
import Domain.Scenarios.ScenarioState

exercises :: IO [(Exercise ScenarioState, FilePath)]
exercises = 
    F.find F.always (F.extension ==? ".bin") root >>= return . map readExercise 
  where root = "bins" :: FilePath-- : The script directory.

readExercise :: FilePath -> (Exercise ScenarioState, FilePath)
readExercise path = (mkExercise id strategy difficulty initialState, path)
  where 
    id = "scenarios" # newId (takeBaseName path)    
    scenario@(Scenario metadata _ dialogue) = readBinaryScenario path
    strategy = makeStrategy (scenarioID metadata) dialogue
    difficulty = scenarioDifficulty metadata
    parameters = scenarioParameters metadata
    processParameter p = (parameterId p, fromMaybe 0 (parameterInitialValue p))
    initialEmotion = 
		maybe empty (\emotion -> case emotion of 
			"" -> empty
			_  -> insert emotion 1 empty)
		(scenarioStartEmotion metadata)
    initialState = ScenarioState (fromList (map processParameter parameters)) initialEmotion emptyStatementInfo
    
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