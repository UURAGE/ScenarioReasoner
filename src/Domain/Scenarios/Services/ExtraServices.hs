{- © Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Services.ExtraServices where

import Control.Arrow
import Data.Maybe

import Ideas.Common.Library
import Ideas.Service.State

import Domain.Scenarios.Expression
import Domain.Scenarios.Globals
import Domain.Scenarios.Scenario
import Domain.Scenarios.ScenarioState
import Domain.Scenarios.Services.Types
import qualified Domain.Scenarios.DomainData as DD

-- ScenarioList and Info Service -------------------------------------------------------------------------------------

-- Scenariolist service: lists all info for each scenario
scenariolist :: [(Id, Scenario)] -> [ScenarioInfo]
scenariolist = map getScenarioInfo

-- Scenarioinfo service: shows the info for a specific scenario (exercise)
scenarioinfo :: [(Id, Scenario)] -> Exercise a -> ScenarioInfo
scenarioinfo fs ex = getScenarioInfo (findScenario "scenarioinfo" fs ex)

getScenarioInfo :: (Id, Scenario) -> ScenarioInfo
getScenarioInfo (sId, ~(Scenario definitions expressions metadata _)) = ScenarioInfo
                sId
                (scenarioName           metadata)
                (scenarioLanguage       metadata)
                (scenarioDescription    metadata)
                (scenarioDifficulty     metadata)
                (scenarioVersion        metadata)
                (map describeCharacterDefinition (definitionsCharacters definitions))
                (map describeDefinition expressions)
                (map describeDefinition (useredUserDefined (fst (definitionsParameters definitions))))
                (scenarioPropertyValues metadata)
  where
    describeCharacterDefinition definition = CharacterDefinitionInfo
        (characterDefinitionId   definition)
        (characterDefinitionName definition)
    describeDefinition definition = DefinitionInfo
        (definitionId          definition)
        (definitionName        definition)
        (definitionDescription definition)
        (definitionType        definition)

evaluate :: [(Id, Scenario)] -> State a -> Assocs DD.Value
evaluate fs st = Assocs (map (definitionId &&& evaluateExpression tm state . definitionContent) exprs)
  where
    ex = exercise st
    scen = snd (findScenario "evaluate" fs ex)
    tm = snd (definitionsParameters (scenarioDefinitions scen))
    exprs = scenarioExpressions scen
    ScenarioState state _ = fromMaybe (error "Cannot evaluate exercise: casting failed.") $
        castFrom ex (stateTerm st) :: ScenarioState

-- | Finds the scenario of the exercise in the given scenario list
findScenario :: String -> [(Id, Scenario)] -> Exercise a -> (Id, Scenario)
findScenario usage scenarios ex = fromMaybe
    (error $ "Cannot " ++ usage ++ " exercise: exercise is apparently not a scenario.")
    (tupleWithId <$> lookup sId scenarios)
  where
    sId = getId ex
    tupleWithId s = (sId, s)
