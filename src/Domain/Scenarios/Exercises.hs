{- © Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Exercises where

import Data.Maybe(fromMaybe)

import Ideas.Common.Library
import Ideas.Encoding.Encoder

import Domain.Scenarios.Strategy(makeStrategy)
import Domain.Scenarios.Scenario
import Domain.Scenarios.ScenarioState

exercises :: [(Id, Scenario)] -> [Exercise ScenarioState]
exercises = map readExercise

-- Pattern match on Scenario must be lazy to preserve laziness!
readExercise :: (Id, Scenario) -> Exercise ScenarioState
readExercise idscen@(sId, ~Scenario { scenarioMetaData = metadata, scenarioTopDialogue = dialogue }) =
    mkExercise sId strat difficulty initialState
  where
    strat      = makeStrategy idscen dialogue
    difficulty = scenarioDifficulty metadata
    initialState = ScenarioState (scenarioInitialParameterValues metadata) Nothing

mkExercise :: Id -> Strategy ScenarioState -> Maybe Difficulty -> ScenarioState -> Exercise ScenarioState
mkExercise sId strat difficulty initState = jsonEncoding
    emptyExercise
       { exerciseId     = sId
       , status         = Alpha
       , parser         = readJSON
       , prettyPrinter  = showJSON
       , equivalence    = \_ _-> True
       , similarity     = \_ _-> True
       , ready          = true
       , suitable       = true
       , hasTypeable    = useTypeable
       , strategy       = liftToContext $ label "Scenario Strategy" strat
       -- Preserve choice order
       , ruleOrdering   = \_ _ -> EQ
       , examples       = examplesWithDifficulty [(fromMaybe Medium difficulty, initState)]
       }
