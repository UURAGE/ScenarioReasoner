module Domain.Scenarios.Services.ServiceList where

import System.FilePath(FilePath)

import Ideas.Common.Library
import Ideas.Service.Types
import Ideas.Service.State

import Domain.Scenarios.Services.ScenarioInfo
import Domain.Scenarios.Services.Score

import Domain.Scenarios.Types

-- A list of all custom services available
customServices :: [FilePath] -> [Service]
customServices fs = map ($ fs)
    [scenariolistS, scenarioinfoS, scoreS]

scenariolistS :: [FilePath] -> Service
scenariolistS fs = makeService "scenarios.scenariolist"
    "Lists all available scenarios." $
    scenariolist fs ::: tList tScenarioInfo

scenarioinfoS :: [FilePath] -> Service
scenarioinfoS fs = makeService "scenarios.scenarioinfo"
    "Returns information about the scenario." $
    scenarioinfo fs ::: tExercise .-> tScenarioInfo

scoreS :: [FilePath] -> Service
scoreS fs = makeService "scenarios.score"
    "Calculates the score of a given state." $
    score fs ::: tState .-> tScoreResult

