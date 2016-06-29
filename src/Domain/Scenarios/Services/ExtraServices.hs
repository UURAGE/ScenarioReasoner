{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Services.ExtraServices where

import Data.Maybe

import Ideas.Common.Library

import Domain.Scenarios.Globals
import Domain.Scenarios.Scenario
import Domain.Scenarios.Services.Types

-- ScenarioList and Info Service -------------------------------------------------------------------------------------

-- Scenariolist service: lists all info for each scenario
scenariolist :: [(Id, Scenario)] -> [ScenarioInfo]
scenariolist = map (getScenarioInfo . snd)

-- Scenarioinfo service: shows the info for a specific scenario (exercise)
scenarioinfo :: [(Id, Scenario)] -> Exercise a -> ScenarioInfo
scenarioinfo fs ex = getScenarioInfo (findScenario "scenarioinfo" fs ex)

getScenarioInfo :: Scenario -> ScenarioInfo
getScenarioInfo (Scenario definitions metadata _) = ScenarioInfo
                (scenarioName           metadata)
                (scenarioDescription    metadata)
                (scenarioDifficulty     metadata)
                (map describeDefinition (useredUserDefined (fst (definitionsParameters definitions))))
                (scenarioPropertyValues metadata)
  where
    describeDefinition definition = DefinitionInfo
        (definitionId          definition)
        (definitionName        definition)
        (definitionDescription definition)
        (definitionType        definition)

-- | Finds the scenario of the exercise in the given scenario list
findScenario :: String -> [(Id, Scenario)] -> Exercise a -> Scenario
findScenario usage scenarios ex = fromMaybe
    (error $ "Cannot " ++ usage ++ " exercise: exercise is apparently not a scenario.")
    (lookup (getId ex) scenarios)
