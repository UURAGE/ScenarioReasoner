module Domain.Scenarios.Services.ServiceList where

import Ideas.Common.Library
import Ideas.Service.Types
import Ideas.Service.State

import Domain.Scenarios.Services.ScenarioInfo
import Domain.Scenarios.Services.StatementsInfo
import Domain.Scenarios.Services.Score

import Domain.Scenarios.Globals(ScriptElement)
import Domain.Scenarios.Types

customServices :: [ScriptElement] -> [Service]
customServices scripts = map (flip ($) scripts)
    [scenariolistS, scenarioinfoS, statementsinfoS, scoreS]

scenariolistS :: [ScriptElement] -> Service
scenariolistS scripts = makeService "scenarios.scenariolist"
    "Lists all available scenarios." $
    (scenariolist scripts) ::: (tList tScenarioInfo)

scenarioinfoS :: [ScriptElement] -> Service
scenarioinfoS scripts = makeService "scenarios.scenarioinfo"
    "Returns information about the scenario." $
    (scenarioinfo scripts) ::: tExercise .-> tScenarioInfo

statementsinfoS :: [ScriptElement] -> Service
statementsinfoS scripts = makeService "scenarios.statementsinfo"
    "Returns information for all statements of the scenario." $
    (statementsinfo scripts) ::: tExercise .-> (tList tStatementInfo)

scoreS :: [ScriptElement] -> Service
scoreS scripts = makeService "scenarios.score"
    "Calculates the score of a given state." $
    (score scripts) ::: tState .-> tScoreResult

