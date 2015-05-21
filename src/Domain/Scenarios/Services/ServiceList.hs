module Domain.Scenarios.Services.ServiceList where

import Ideas.Common.Library
import Ideas.Service.Types
import Ideas.Service.State

import Domain.Scenarios.Services.ScenarioInfo
import Domain.Scenarios.Services.Score

import Domain.Scenarios.Globals(Script)
import Domain.Scenarios.Types

-- A list of all custom services available
customServices :: [Script] -> [Service]
customServices scripts = map ($ scripts)
    [scenariolistS, scenarioinfoS, scoreS]

scenariolistS :: [Script] -> Service
scenariolistS scripts = makeService "scenarios.scenariolist"
    "Lists all available scenarios." $
    scenariolist scripts ::: tList tScenarioInfo

scenarioinfoS :: [Script] -> Service
scenarioinfoS scripts = makeService "scenarios.scenarioinfo"
    "Returns information about the scenario." $
    scenarioinfo scripts ::: tExercise .-> tScenarioInfo

scoreS :: [Script] -> Service
scoreS scripts = makeService "scenarios.score"
    "Calculates the score of a given state." $
    score scripts ::: tState .-> tScoreResult

