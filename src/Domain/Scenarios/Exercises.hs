{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Exercises where

import Data.Map(fromList)
import Data.Maybe(fromMaybe)

import Ideas.Common.Library

import Domain.Scenarios.Globals
import Domain.Scenarios.Strategy(makeStrategy)
import Domain.Scenarios.Scenario
import Domain.Scenarios.ScenarioState

exercises :: [(Id, Scenario)] -> [Exercise ScenarioState]
exercises = map readExercise

readExercise :: (Id, Scenario) -> Exercise ScenarioState
readExercise (sId, Scenario metadata _ dialogue) = mkExercise sId strat difficulty initialState
  where
    strat      = makeStrategy (showId sId) dialogue
    difficulty = scenarioDifficulty metadata
    parameters = scenarioParameters metadata
    processParameter p = (parameterId p, fromMaybe 0 (parameterInitialValue p))
    initialParameters = fromList (map processParameter parameters)
    initialState = ScenarioState initialParameters Nothing

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
