{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Domain.Scenarios.Services.ServiceList where

import Ideas.Common.Library
import Ideas.Service.Types
import Ideas.Service.State

import Domain.Scenarios.Parser
import Domain.Scenarios.Services.ScenarioInfo(scenariolist, scenarioinfo, tScenarioInfo)
import Domain.Scenarios.Services.StatementsInfo(statementsinfo, tStatementInfo)
import Domain.Scenarios.Services.Score(score, tScoreResult)

customServices :: [Script] -> [Service]
customServices scripts = map (flip ($) scripts)
    [scenariolistS, scenarioinfoS, statementsinfoS, scoreS]

scenariolistS :: [Script] -> Service
scenariolistS scripts = makeService "scenarios.scenariolist"
    "Lists all available scenarios." $
    (scenariolist scripts) ::: (tList tScenarioInfo)

scenarioinfoS :: [Script] -> Service
scenarioinfoS scripts = makeService "scenarios.scenarioinfo"
    "Returns information about the scenario." $
    (scenarioinfo scripts) ::: tExercise .-> tScenarioInfo

statementsinfoS :: [Script] -> Service
statementsinfoS scripts = makeService "scenarios.statementsinfo"
    "Returns information for all statements of the scenario." $
    (statementsinfo scripts) ::: tExercise .-> (tList tStatementInfo)

scoreS :: [Script] -> Service
scoreS scripts = makeService "scenarios.score"
    "Calculates the score of a given state." $
    (score scripts) ::: tState .-> tScoreResult

